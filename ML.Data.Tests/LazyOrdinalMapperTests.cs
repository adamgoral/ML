namespace ML.Data.Tests
{
    using System;
    using Microsoft.VisualStudio.TestTools.UnitTesting;

    [TestClass]
    public class LazyOrdinalMapperTests
    {

        [TestMethod]
        public void MapSourceParameterMustNotBeNull()
        {
            // Arrange
            var mapper = new LazyOrdinalMapper<object>();

            // Act
            var exception = AssertX.Throws<ArgumentNullException>(() => mapper.Map(null));

            // Assert
            Assert.AreEqual("source", exception.ParamName);
        }

        [TestMethod]
        public void FirstMappedValueIs0()
        {
            // Arrange
            var mapper = new LazyOrdinalMapper<object>();

            // Act
            var actual = mapper.Map(new object());

            // Assert
            Assert.AreEqual(0, actual);
        }

        [TestMethod]
        public void DuplicateInputMapsToSameCode()
        {
            // Arrange
            var mapper = new LazyOrdinalMapper<string>();

            // Act
            var firstCode = mapper.Map("firstValue");
            var secondCode = mapper.Map("secondValue");
            var thirdCode = mapper.Map("firstValue");

            // Assert
            Assert.AreEqual(firstCode, thirdCode);
        }

        [TestMethod]
        public void SubsequentInputMapsToIncrementalCode()
        {
            // Arrange
            var mapper = new LazyOrdinalMapper<string>();

            // Act
            var firstCode = mapper.Map("firstValue");
            var secondCode = mapper.Map("secondValue");
            var thirdCode = mapper.Map("thirdCode");

            // Assert
            Assert.AreEqual(firstCode + 1, secondCode);
            Assert.AreEqual(secondCode + 1, thirdCode);
        }
    }
}
