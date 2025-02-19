package ca.jrvs.apps.stockquote.config;

import java.io.*;
import java.util.HashMap;
import java.util.Map;

public class ConfigLoader {
    private Map<String, String> properties = new HashMap<>();

    public ConfigLoader(String filePath) {
        loadProperties(filePath);
    }

    private void loadProperties(String filePath) {
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] tokens = line.split("=");
                if (tokens.length == 2) {
                    properties.put(tokens[0].trim(), tokens[1].trim());
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public String getProperty(String key) {
        return properties.get(key);
    }
}
