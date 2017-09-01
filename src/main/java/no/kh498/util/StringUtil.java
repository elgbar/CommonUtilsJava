package no.kh498.util;

import java.util.regex.Pattern;

import static java.lang.String.valueOf;

/**
 * @author kh498
 */
@SuppressWarnings({"SameParameterValue", "unused"})
public class StringUtil {

    /**
     * Make a number easier to read by placing spaces every three characters
     * <p>
     * Example: {@code 100000} -> {@code 100 000}
     *
     * @param number
     *     the number to beautify
     *
     * @return A String of the number that is easier to read
     */
    public static String beautify(final int number) {
        final String[] sa = valueOf(number).split("");
        if (sa.length <= 3) {
            return number + "";
        }

        final StringBuilder sb = new StringBuilder();
        for (int i = sa.length - 1; i >= 0; i--) {
            sb.append(sa[(sa.length - 1) - i]);
            if (i % 3 == 0) {
                sb.append(' ');
            }
        }
        removeTail(sb);
        return sb.toString();
    }

    /**
     * Format a string so it's using the correct plural ending
     * <p>
     * <b>for example </b>
     * <p>
     * kh -> kh's
     * <p>
     * Mads -> Mads'
     *
     * @param s
     *     String to check
     *
     * @return The string with the proper ending
     *
     * @throws NullPointerException
     *     is the string is null
     */
    public static String ending(final String s) {
        if (s == null) {
            throw new IllegalArgumentException("The argument cannot be null!");
        }
        char lastChar = s.toCharArray()[s.length() - 1];
        return s + '\'' + (equalsIgnoreCase('s', lastChar) ? "" : 's');
    }

    private static Pattern VOWEL = Pattern.compile(" [aeiouyAEIOUY]");

    /**
     * @param followingWord
     *     The word to follow
     *
     * @return A String with either 'a' or 'an' before it depending on the first character of {@code followingWord}
     */
    public static String AAn(String followingWord) {
        boolean startVowel = VOWEL.matcher(followingWord).find();
        return "a" + (startVowel ? 'n' : "") + " " + followingWord;
    }

    /**
     * Check if two characters are equal ignoring the case
     *
     * @param c1
     *     A char
     * @param c2
     *     Another char
     *
     * @return {@code true} if the argument represents an equivalent {@code String} ignoring case; {@code false}
     * otherwise
     */
    static boolean equalsIgnoreCase(final char c1, final char c2) {
        return Character.toUpperCase(c1) == Character.toUpperCase(c2);
    }

    /**
     * Remove {@code amount} of character from the end of {@code sb}
     *
     * @param sb
     *     The StringBuilder to modify
     * @param amount
     *     The amount of character to remove
     *
     * @return {@code sb}
     */
    public static StringBuilder removeTail(StringBuilder sb, int amount) {
        if (sb.length() >= amount) {
            sb.setLength(sb.length() - amount);
        }
        return sb;
    }

    /**
     * Shorten the {@code sb} with one character
     *
     * @param sb
     *     The StringBuilder to modify
     *
     * @return {@code sb}
     */
    public static StringBuilder removeTail(StringBuilder sb) {
        return removeTail(sb, 1);
    }
}
