using Newtonsoft.Json;
using System.IO;

namespace TestsIntegration
{
    class Settings
    {
        public string TestDir { get; set; }

        public static Settings GetFromFile()
        {
            using (var file = new FileStream("settings.json", FileMode.Open, FileAccess.Read))
            using (var reader = new StreamReader(file))
            {
                var content = reader.ReadToEnd();
                var settings = JsonConvert.DeserializeObject<Settings>(content);

                return settings;
            }
        }
    }
}
