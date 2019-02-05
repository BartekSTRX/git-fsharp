using GitLib;
using System;
using System.IO;
using Xunit;

namespace TestsIntegration
{
    public class InitTests : IDisposable
    {
        private readonly string repoPath;

        public InitTests()
        {
            var repoDirName = "testRepo";
            repoPath = Path.Combine(Config.DirPath, repoDirName);
        }

        [Fact]
        public void Test1()
        {
            Commands.init(repoPath);

            Assert.True(Directory.Exists(repoPath));
        }

        public void Dispose()
        {
            Directory.Delete(repoPath, recursive: true);
        }
    }
}
