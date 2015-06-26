package ru.vitkud.delphi.java;

import ru.vitkud.delphi.DelphiType;

public class DjType {
	String javaType;

	public DjType(DelphiType delphiType) {
		javaType = delphiType.getName();
		if (javaType.equalsIgnoreCase("string"))
			javaType = "String";
		if (javaType.equalsIgnoreCase("WideString"))
			javaType = "String";
		if (javaType.equalsIgnoreCase("AnsiString"))
			javaType = "byte[]";
		if (javaType.equalsIgnoreCase("RawByteString"))
			javaType = "byte[]";
		else if (javaType.equalsIgnoreCase("Integer"))
			javaType = "int";
		else if (javaType.equalsIgnoreCase("Cardinal"))
			javaType = "int";
		else if (javaType.equalsIgnoreCase("Word"))
			javaType = "short";
		else if (javaType.equalsIgnoreCase("TDateTime"))
			javaType = "Date";
		else if (javaType.equalsIgnoreCase("TDateTime"))
			javaType = "Date";
		// TODO ...
	}

	@Override
	public String toString() {
		return javaType;
	}
}

