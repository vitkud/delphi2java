package ru.vitkud.delphi.java;

public final class DjHelper {

	public static String delphiNameToJava(String name) {
		StringBuilder result = new StringBuilder(name.length());
		boolean prevUpper = false;
		if (Character.isUpperCase(name.charAt(0))) {
			result.append(Character.toLowerCase(name.charAt(0)));
			prevUpper = true;
		} else {
			result.append(name.charAt(0));
		}
		for (int i = 1; i < name.length(); ++i) {
			if (Character.isUpperCase(name.charAt(i))) {
				if (i + 1 == name.length()) {
					if (prevUpper) {
						result.append(Character.toLowerCase(name.charAt(i)));
					} else {
						result.append(name.charAt(i));
					}
				} else {
					boolean nextUpper = Character.isUpperCase(name.charAt(i + 1));
					if (prevUpper) {
						if (nextUpper) {
							result.append(Character.toLowerCase(name.charAt(i)));
						} else {
							result.append(name.charAt(i));
						}
					} else {
						result.append(name.charAt(i));
					}
				}
				prevUpper = true;
			} else {
				result.append(name.charAt(i));
				prevUpper = false;
			}
		}
		return result.toString();
	}

}
