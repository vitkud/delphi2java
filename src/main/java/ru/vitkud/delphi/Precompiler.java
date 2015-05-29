package ru.vitkud.delphi;

import java.io.FilterInputStream;
import java.io.InputStream;
import java.util.Set;

public class Precompiler extends FilterInputStream {

	public Precompiler(InputStream in, Set<String> definitions) {
		super(in);
	}

	// TODO Implement preprocessor (resolve Delphi compiler directives)
	// I, INCLUDE
	// DEFINE, UNDEF
	// IFDEF, IFNDEF, ELSE, ENDIF
	// IF, ELSEIF, IFEND
}
