namespace ML.Data.Tests
{
    using System;
    using System.Collections.Generic;

    using Deedle;
    using Microsoft.VisualStudio.TestTools.UnitTesting;

    [TestClass]
    public class CodebookTests
    {
        [TestMethod]
        public void EncodeFrameSourceParameterCannotBeNull()
        {
            // Arrange
            var codebook = new Codebook<string>(new Dictionary<string, IMapper>());
            Frame<int, string> frame = null;

            // Act
            var exception = AssertX.Throws<ArgumentNullException>(() => codebook.Encode(frame));

            // Assert
            Assert.AreEqual("source", exception.ParamName);
        }

        [TestMethod]
        public void EncodeFrameThrowsExceptionIfFrameHasColumnWithoutMapper()
        {
            // Arrange
            var codebook = new Codebook<int>(new Dictionary<int, IMapper>());
            var frame = Frame.FromArray2D(new object[2,2]);

            // Act
            var exception = AssertX.Throws<InvalidOperationException>(() => codebook.Encode(frame));

            // Assert
        }

        [TestMethod]
        [Ignore]
        public void EncodeFrameReturnsExpectedCodes()
        {
            // Arrange
            var expectedCodes = new int[2, 2];
            expectedCodes[1, 0] = 0;
            expectedCodes[1, 1] = 1;
            var expectedFrame = Frame.FromArray2D(expectedCodes);
            var codebook = new Codebook<string>(new Dictionary<string, IMapper> 
            {
                {"col1", new LazyOrdinalMapper<string>()},
                {"col2", new LazyOrdinalMapper<string>()}
            });
            var inputFrame = Frame.FromRecords(new[] { new { col1 = "a", col2 = "a" }, new { col1 = "a", col2 = "b" } });

            // Act
            var encodedFrame = codebook.Encode(inputFrame);

            // Assert
            var actualCodes = encodedFrame.ToArray2D<int>();
            Assert.AreEqual(expectedCodes.Length, actualCodes.Length);
            Assert.AreEqual(expectedCodes.GetLength(0), actualCodes.GetLength(0));
            Assert.AreEqual(expectedCodes.GetLength(1), actualCodes.GetLength(1));
            Assert.AreEqual(expectedCodes[0, 0], actualCodes[0, 0]);
            Assert.AreEqual(expectedCodes[1, 0], actualCodes[1, 0]);
            Assert.AreEqual(expectedCodes[0, 1], actualCodes[0, 1]);
            Assert.AreEqual(expectedCodes[1, 1], actualCodes[1, 1]);
        }
    }
}
