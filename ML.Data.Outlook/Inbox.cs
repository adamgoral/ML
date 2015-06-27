namespace ML.Data.Outlook
{
    using Microsoft.Office.Interop.Outlook;
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.IO;
    using System.Linq;
    using System.Runtime.InteropServices;
    using System.Text.RegularExpressions;
    using System.Threading.Tasks;
    using System.Runtime.Serialization.Formatters.Binary;

    public class Inbox
    {
        public class CategorisedItem
        {
            public CategorisedItem(string id, string category)
                : this(id, new[] { category })
            {
            }

            public CategorisedItem(string id, IEnumerable<string> categories)
            {
                this.Id = id;
                this.Categories = categories.ToArray();
            }

            public string Id { get; private set; }
            public string[] Categories { get; private set; }
        }

        public class Mail : CategorisedItem
        {
            public Mail(string id, string from, string subject, string body, IEnumerable<string> categories) : base(id, categories)
            {
                this.From = from;
                this.Subject = subject;
                this.Body = body;
            }

            public string From { get; private set; }
            public string Subject { get; private set; }
            public string Body { get; private set; }
        }

        public static Application GetApplication()
        {
            var outlookProcesses = Process.GetProcessesByName("OUTLOOK");
            if (outlookProcesses.Any())
            {
                return Marshal.GetActiveObject("Outlook.Application") as Application;
            }

            throw new InvalidOperationException("No outlook running");
        }

        public static IEnumerable<Mail> GetUnprocessedMail()
        {
            var app = GetApplication();
            var session = app.Session;

            var inboxFolder = session.GetDefaultFolder(OlDefaultFolders.olFolderInbox);
            var mail = GetMail(inboxFolder);
            return mail;
        }

        public static IEnumerable<Mail> GetProcessedMail()
        {
            return GetProcessedMailFolders().SelectMany(f => f.Mail);
        }

        public class MailFolder
        {
            private readonly MAPIFolder mapiFolder;

            public MailFolder(MAPIFolder mapiFolder)
            {
                this.mapiFolder = mapiFolder;
            }

            public string Name { get { return this.mapiFolder.Name; } }

            public IEnumerable<Mail> Mail
            {
                get { return GetMail(this.mapiFolder); }
            }
        }

        public static IEnumerable<MailFolder> GetProcessedMailFolders()
        {
            var app = GetApplication();
            var session = app.Session;

            var junkFolder = session.GetDefaultFolder(OlDefaultFolders.olFolderJunk);
            var mail = GetMail(junkFolder);

            var categorisedFolders = GetSubFolders(session.GetDefaultFolder(OlDefaultFolders.olFolderInbox));
            return categorisedFolders.Select(f => new MailFolder(f));
        }

        private static Regex WordsRegEx = new Regex(@"[a-zA-Z@#][a-zA-Z'\-@.]+[a-zA-Z]+");

        public static IEnumerable<string> GetWords(string source)
        {
            if (string.IsNullOrWhiteSpace(source))
                yield break;

            foreach (var match in WordsRegEx.Matches(source))
            {
                var m = match as Match;
                if (m != null)
                {
                    yield return m.Value;
                }
            }
        }

        public static HashSet<string> WordDictionary = GetWordDictionary();

        public static HashSet<string> GetWordDictionary()
        {
            if (!File.Exists("oxfordenglishdictionary.txt"))
            {
                return new HashSet<string>();
            }

            return new HashSet<string>(GetWords(File.ReadAllText("oxfordenglishdictionary.txt"))
                .Where(s => s.Length > 0)
                .Select(s => s.ToLower()));
        }

        public static Dictionary<string, Dictionary<string, int>> GetWordCountsByCategory(IEnumerable<Tuple<IEnumerable<string>, IEnumerable<string>>> source)
        {
            var result = new Dictionary<string, Dictionary<string, int>>();
            foreach(var item in source)
            {
                foreach(var cat in item.Item2)
                {
                    Dictionary<string, int> catStat = null;
                    if (!result.TryGetValue(cat, out catStat))
                    {
                        catStat = new Dictionary<string, int>();
                        result.Add(cat, catStat);
                    }

                    foreach(var w in item.Item1)
                    {
                        var val = 0;
                        catStat.TryGetValue(w, out val);
                        val++;
                        catStat[w] = val;
                    }
                }
            }

            return result;
        }

        public static Dictionary<string, int> GetWordCounts(IEnumerable<string> items)
        {
            return items.GroupBy(k => k).ToDictionary(g => g.Key, g => g.Count());
        }

        public static Dictionary<string, int> GetWordCounts(IEnumerable<IEnumerable<string>> items)
        {
            return GetWordCounts(items.SelectMany(item => item));
        }

        private static IEnumerable<string> ParseCategories(string source)
        {
            if (string.IsNullOrWhiteSpace(source))
                return new string[0];

            return source.Split(',').Select(s => s.Trim());
        }

        public static Task SetCategories(IEnumerable<CategorisedItem> items)
        {
            var app = GetApplication();
            var session = app.Session;

            return Task.Factory.StartNew(() => 
            {
                foreach(var item in items)
                {
                    var existingItem = session.GetItemFromID(item.Id) as MailItem;
                    if (existingItem != null)
                    {
                        var existingCategories = existingItem.Categories ?? string.Empty;
                        var newValue = string.Join(", ", item.Categories);
                        if (existingCategories != newValue)
                        {
                            existingItem.Categories = newValue;
                            existingItem.Save();
                        }
                        else
                        {
                            existingItem.Close(OlInspectorClose.olDiscard);
                        }

                        Marshal.FinalReleaseComObject(existingItem);
                    }
                }
            });
        }

        public static Task CategoriseAllMail(Func<Mail, IEnumerable<string>> categoriser)
        {
            return Task.Factory.StartNew(() => 
            {
                var app = GetApplication();
                var session = app.Session;

                var inboxFolder = session.GetDefaultFolder(OlDefaultFolders.olFolderInbox);
                var junkFolder = session.GetDefaultFolder(OlDefaultFolders.olFolderJunk);
                var categorisedFolders = new List<MAPIFolder>(GetSubFolders(inboxFolder));
                categorisedFolders.Add(inboxFolder);
                categorisedFolders.Add(junkFolder);
                var mailItems = categorisedFolders.SelectMany(folder => GetMailItems(folder));
                foreach(var mailItem in mailItems)
                {
                    var existingCategories = ParseCategories(mailItem.Categories).OrderBy(k => k).ToArray();
                    var mail = new Mail(mailItem.EntryID, mailItem.SenderName, mailItem.Subject, mailItem.Body, existingCategories);
                    var newCategories = string.Join(", ", categoriser(mail).OrderBy(k => k).ToArray());
                    if(string.Join(", ", existingCategories) != newCategories)
                    {
                        mailItem.Categories = newCategories;
                        mailItem.Save();
                    }
                    else
                    {
                        mailItem.Close(OlInspectorClose.olDiscard);
                    }

                    Marshal.FinalReleaseComObject(mailItem);
                }
            });
        }

        private static T GetValueOrDefault<T>(IDictionary<string, T> dict, string key, T defaultValue)
        {
            T result = default(T);
            if(!dict.TryGetValue(key, out result))
            {
                return defaultValue;
            }

            return result;
        }

        public static Task SaveWordCounts(IDictionary<string, Dictionary<string, int>> wordsByCategory, string fileName)
        {
            return Task.Factory.StartNew(() =>
            {
                var words = new HashSet<string>(wordsByCategory.SelectMany(kvp => kvp.Value.Keys));
                var categories = wordsByCategory.Keys.ToArray();
                using (var writer = new StreamWriter(File.OpenWrite(fileName)))
                {
                    writer.WriteLine("," + string.Join(",", categories));
                    var parts = new List<string>();
                    foreach (var word in words)
                    {
                        foreach (var cat in categories)
                        {
                            var value = GetValueOrDefault(wordsByCategory[cat], word, 0);
                            parts.Add(value.ToString());
                        }

                        writer.WriteLine(word + "," + string.Join(",", parts));
                    }
                }
            });
        }

        public static Task Serialize<T>(T obj, string fileName)
        {
            return Task.Factory.StartNew(() =>
            {
                if (File.Exists(fileName))
                {
                    File.Delete(fileName);
                }

                using (var writer = File.OpenWrite(fileName))
                {
                    var formatter = new BinaryFormatter();
                    formatter.Serialize(writer, obj);
                }
            });
        }

        public static Task<T> Deserialize<T>(string fileName)
        {
            return Task.Factory.StartNew(() => 
            {
                using(var reader = File.OpenRead(fileName))
                {
                    var formatter = new BinaryFormatter();
                    return (T) formatter.Deserialize(reader);
                }
            });
        }

        public static Task<Dictionary<string, Dictionary<string, int>>> LoadWordCounts(string fileName)
        {
            return Task.Factory.StartNew(() => 
            {
                var result = new Dictionary<string, Dictionary<string, int>>();
                using(var reader = new StreamReader(File.OpenRead(fileName)))
                {
                    var categories = reader.ReadLine().Split(',').Skip(1).ToArray();
                    foreach(var category in categories)
                    {
                        result.Add(category, new Dictionary<string, int>());
                    }

                    while(!reader.EndOfStream)
                    {
                        var parts = reader.ReadLine().Split(',');
                        var word = parts[0];
                        var values = parts.Skip(1).Select(int.Parse).ToArray();
                        for(var i = 0; i<categories.Length; i++)
                        {
                            var category = categories[i];
                            result[category][word] = values[i];
                        }
                    }
                }

                return result;
            });
        }

        public static IEnumerable<MailItem> GetMailItems(MAPIFolder folder)
        {
            foreach(var item in folder.Items)
            {
                var mailItem = item as MailItem;
                if(mailItem != null)
                {
                    yield return mailItem;
                }
            }
        }

        public static IEnumerable<Mail> GetMail(MAPIFolder folder)
        {
            var categories = GetCategoriesFromFolderPath(folder).ToArray();
            return GetMailItems(folder).Select(mailItem =>
                {
                    var result = new Mail(mailItem.EntryID, mailItem.SenderName, mailItem.Subject, mailItem.Body, categories);
                    Marshal.FinalReleaseComObject(mailItem);
                    return result;
                });
        }

        private static IEnumerable<string> GetCategoriesFromFolderPath(MAPIFolder folder)
        {
            var current = folder;
            var inboxFolder = folder.Session.GetDefaultFolder(OlDefaultFolders.olFolderInbox);
            var rootFolder = folder.Session.GetDefaultFolder(OlDefaultFolders.olFolderInbox).Parent as MAPIFolder;

            while (current != null && current.EntryID != rootFolder.EntryID && current.EntryID != inboxFolder.EntryID)
            {
                yield return current.Name;
                current = current.Parent as MAPIFolder;
            }
        }

        public static IEnumerable<MAPIFolder> GetSubFolders(MAPIFolder folder)
        {
            foreach(var subfolder in folder.Folders)
            {
                var mapiFolder = subfolder as MAPIFolder;
                if(mapiFolder != null)
                {
                    yield return mapiFolder;
                    foreach (var item in GetSubFolders(mapiFolder))
                        yield return item;
                }
            }
        }
    }
}
