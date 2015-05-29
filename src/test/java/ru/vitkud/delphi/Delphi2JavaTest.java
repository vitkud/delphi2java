package ru.vitkud.delphi;

import org.junit.Test;

import java.util.HashSet;

public class Delphi2JavaTest {

	@Test
	public void testConvert() throws Exception {
		Delphi2Java.convert(this.getClass().getResourceAsStream("TestUnitU.pas"),
				new String[]{"VER220", "RELEASE"});
	}

}
