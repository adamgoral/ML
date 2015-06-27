namespace ML.Data.Tests
{
    using System.Collections.Generic;
    using Microsoft.VisualStudio.TestTools.UnitTesting;

    [TestClass]
    public class MapperTests
    {
        [TestMethod]
        public void CreatedDecoderMapsToInput()
        {
            // Arrange
            var firstValue = "firstValue";
            var secondValue = "secondValue";
            var firstCode = 0;
            var secondCode = 1;

            var lookup = new Dictionary<string, int>
            {
                {firstValue, firstCode},
                {secondValue, secondCode}
            };

            var mapper = new Mapper<string, int>(lookup);

            // Act
            var decoder = mapper.CreateDecoder();

            // Assert
            Assert.AreEqual(firstValue, decoder.Map(firstCode));
            Assert.AreEqual(secondValue, decoder.Map(secondCode));
        }

        [TestMethod]
        public void InputCastAsObjectMapsToSameCode()
        {
            // Arrange
            var firstValue = "firstValue";
            var firstCode = 0;
            var lookup = new Dictionary<string, int>
            {
                {firstValue, firstCode}
            };

            var mapper = new Mapper<string, int>(lookup);

            // Act
            var firstResult = mapper.Map(firstValue);
            var secondResult = mapper.Map((object)firstValue);

            // Assert
            Assert.AreEqual(firstCode, firstResult);
            Assert.IsInstanceOfType(secondResult, typeof(int));
            Assert.AreEqual(firstCode, (int)secondResult);
        }
    }
}
