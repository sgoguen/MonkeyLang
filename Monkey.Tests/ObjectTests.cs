using Xunit;
using Monkey.Core;

namespace Monkey.Tests
{
    public class ObjectTests
    {        
        [Fact]
        public void TestStringHashKey()
        {
            var hello1 = new MonkeyString { Value = "Hello world" };
            var hello2 = new MonkeyString { Value = "Hello world" };
            var diff1 = new MonkeyString { Value = "My name is johnny" };
            var diff2 = new MonkeyString { Value = "My name is johnny" };

            // General rule for every hashable types: object with same content
            // have different hash keys.
            Assert.Equal(hello1.HashKey(), hello2.HashKey());
  
            // General rule for every hashable type: object with same content
            // have different hash keys.
            Assert.Equal(diff1.HashKey(), diff2.HashKey());

            // General rule for every hashable type: object with different
            // content have same hash keys.
            Assert.NotEqual(hello1.HashKey(), diff1.HashKey());
        }

        [Fact]
        public void TestBooleanHashKey()
        {
            var true1 = new MonkeyBoolean { Value = true };
            var true2 = new MonkeyBoolean { Value = true };
            var false1 = new MonkeyBoolean { Value = false };
            var false2 = new MonkeyBoolean { Value = false  };

            Assert.Equal(true1.HashKey(), true2.HashKey());
            Assert.Equal(false1.HashKey(), false2.HashKey());
            Assert.NotEqual(true1.HashKey(), false1.HashKey());
        }

        [Fact]
        public void TestIntegerHashKey()
        {
            var one1 = new MonkeyInteger { Value = 1 };
            var one2 = new MonkeyInteger { Value = 1 };
            var two1 = new MonkeyInteger { Value = 2 };
            var two2 = new MonkeyInteger { Value = 2 };

            Assert.Equal(one1.HashKey(), one2.HashKey());
            Assert.Equal(two1.HashKey(), two2.HashKey());
            Assert.NotEqual(one1.HashKey(), two1.HashKey());
        }
    }
}