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
            var settings = Settings.GetFromFile();

            var repoDirName = "testRepo";
            repoPath = Path.Combine(settings.TestDir, repoDirName);
        }

        [Fact]
        public void InitRepository()
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
