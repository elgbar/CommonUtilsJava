package no.kh498.util;

import static java.lang.String.valueOf;

/**
 * @author kh498
 */
public class StringUtil {
    /**
     * Make a number easier to read by placing spaces every three characters
     * <p>
     * example: {@code 100000} -> {@code 100 000}
     *
     * @param number the number
     *
     * @return A string of the number that is easier to read
     */
    public static String beautify(final int number) {
        final String[] sa = valueOf(number).split("");
        if (sa.length <= 3) { return number + ""; }

        final StringBuilder sb = new StringBuilder();
        for (int i = sa.length - 1; i >= 0; i--) {
            sb.append(sa[(sa.length - 1) - i]);
            if (i % 3 == 0) { sb.append(" "); }
        }

        return sb.toString().trim();
    }
    /**
     * Format a string so it's using the correct plural ending
     * <p>
     * <b>for example </b>
     * <p>
     * kh -> kh's
     * <p>
     * mads -> mads'
     *
     * @param s String to check
     *
     * @return The string with the proper ending
     *
     * @throws NullPointerException is the string is null
     */
    public static String ending(final String s) {
        if (s == null) {
            throw new IllegalArgumentException("The argument cannot be null!");
        }
        return s + "'" + ("s".equalsIgnoreCase(s.substring(s.length() - 1)) ? "" : "s");
    }
    /**
     * Check if two characters are equal ignoring the case
     *
     * @param c1 A char
     * @param c2 Another char
     *
     * @return {@code true} if the argument
     * represents an equivalent {@code String} ignoring case; {@code
     * false} otherwise
     */
    public static boolean equalsIgnoreCase(final char c1, final char c2) {
        return Character.toUpperCase(c1) == Character.toUpperCase(c2);
    }
}
