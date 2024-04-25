package org.example;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.Token;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Main {
    public static String readFileToString(String filePath) {
        StringBuilder inputBuilder = new StringBuilder();
        try (Scanner scanner = new Scanner(new File(filePath))) {
            while (scanner.hasNextLine()) {
                inputBuilder.append(scanner.nextLine()); // Read each line and append to the StringBuilder
            }
        } catch (FileNotFoundException e) {
            System.err.println("File not found: " + e.getMessage());
            return null; // Return null if the file is not found
        }
        return inputBuilder.toString(); // Convert StringBuilder to String
    }
    public static void main(String[] args) {
//        // Create an input stream from a stringtemplate
//        String filePath = args[1];
//        String input = readFileToString(filePath);
        String input  = "dev\n" +
                "{\n" +
                " if (bool(true))\n" +
                "    {tout (int(5))}\n" +
                " otherwise\n" +
                "    { tout(charr(\"kirtan\"))}\n" +
                " }\n" +
                " lang";
        CharStream inputStream = CharStreams.fromString(input);

        //generateLexer("src/main/java/org/example/CustomLanguageLexer.g4", "src/main/java/org/example/generated");
            // Create a lexer that reads from the input stream
        CustomLanguageLexer lexer= new CustomLanguageLexer(inputStream);

        // Iterate over the tokens produced by the lexer
        Token token;
        List<String> tokenList = new ArrayList<>();
        do {
            token = lexer.nextToken();
            if(token.getType() != Token.EOF) {
                tokenList.add("'" + token.getText() + "'");
            }
            System.out.println(tokenList);
        } while (token.getType() != Token.EOF);
        writeOutput(tokenList,"src/main/resources/output/output.txt");

   }

   public static void writeOutput(List<String> tokenList,String filePath)
   {
       // Specify the file path

       // Write data to the file
       try (BufferedWriter writer = new BufferedWriter(new FileWriter(filePath))) {
           writer.write("["+tokenList.getFirst());
           for (int i=1;i<tokenList.size();++i) {
               writer.write(","+tokenList.get(i));
           }
           writer.write("]");
           System.out.println("Data has been written to the file successfully.");
       } catch (IOException e) {
           System.err.println("Error writing to the file: " + e.getMessage());
       }
   }


//    public static void generateLexer(String grammarFileName, String outputDirectory) {
//
//            String[] arguments = { "-o", outputDirectory, grammarFileName };
//            // Create an instance of ANTLR Tool
//            Tool tool = new Tool(arguments);
//            // Invoke the ANTLR tool to generate lexer code
//            tool.processGrammarsOnCommandLine();
//
//    }
}

