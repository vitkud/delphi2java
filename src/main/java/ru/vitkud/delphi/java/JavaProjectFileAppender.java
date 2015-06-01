package ru.vitkud.delphi.java;

public interface JavaProjectFileAppender {
	void appendMainJava(String fullPackage, String className, String content);
	void appendTestJava(String fullPackage, String className, String content);
}
