namespace ML.Data.Tests
{
    using System;
    using Microsoft.VisualStudio.TestTools.UnitTesting;

    public static class AssertX
    {
        public static TException Throws<TException>(Action action) where TException : Exception
        {
            try
            {
                action();
                Assert.Fail("No exception thrown.");
            }
            catch (TException ex)
            {
                return ex;
            }
            catch (Exception ex)
            {
                Assert.Fail("Excpetect exception of type {0}, actual exception thrown {1}", typeof(TException).Name, ex.GetType().Name);
            }

            return null;
        }
    }
}
