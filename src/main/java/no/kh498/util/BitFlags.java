package no.kh498.util;

/**
 * A bitmask util to simplify bit flipping and aims to emulate std::bitset methods from c++.
 * <p>
 * Learn more <a href=http://www.learncpp.com/cpp-tutorial/3-8a-bit-flags-and-bit-masks/>here</a>
 * (it's for c++ but the same principles and bitwise operators applies in java)
 *
 * @author karl henrik
 */
@SuppressWarnings({"unused", "WeakerAccess"})
public class BitFlags {

    /**
     * @param flags The flags to check
     * @param mask  The mask to check
     *
     * @return {@code true} if the bit at {@code mask} is equal to the bit at the same position in {@code flags}
     */
    public static boolean test(final int flags, final int mask) {
        //if flags & mask return mask then the mask must have been true
        return (flags & mask) == mask;
    }

    /**
     * @param flags The flags to check
     * @param mask  The mask to check
     *
     * @return If and only if {@code flags} is equal to mask return {@code true}
     */
    public static boolean testOnly(final int flags, final int mask) {
        return flags == mask;
    }

    /**
     * @param flags The flags to change
     * @param mask  The mask to enable
     *
     * @return modified {@code flags} with the bit indicated by {@code masks} enabled
     */
    public static int set(final int flags, final int mask) {
        return flags | mask;
    }

    /**
     * @param flags The flags to change
     * @param masks The masks to enable
     *
     * @return modified {@code flags} with the bits indicated by {@code masks} enabled
     */
    public static int set(final int flags, final int... masks) {
        return flags | combineMasks(masks);
    }

    /**
     * @param flags The flags to change
     * @param mask  The mask to disable
     *
     * @return modified {@code flags} with the bit indicated by {@code masks} disabled
     */
    public static int reset(final int flags, final int mask) {
        //Flip mask (eks ~0001 -> 1110) use bitwise OR (eks 1001 & 1110 -> 1000) to turn off the flag
        return flags & ~mask;
    }
    /**
     * @param flags The flags to change
     * @param masks The masks to disable
     *
     * @return mmodified {@code flags} with the bits indicated by {@code masks} disabled
     */
    public static int reset(final int flags, final int... masks) {
        return flags & ~combineMasks(masks);
    }

    /**
     * @param flags The flags to change
     * @param mask  The mask to be toggled
     *
     * @return modified {@code flags} with the bit indicated by {@code mask} flipped
     */
    public static int flip(final int flags, final int mask) {
        return flags ^ mask;
    }

    /**
     * @param flags The flags to change
     * @param masks The masks to be toggled
     *
     * @return modified {@code flags} with the bits indicated by {@code masks} flipped
     */
    public static int flip(final int flags, final int... masks) {
        return flags ^ combineMasks(masks);
    }

    /**
     * @param masks The mask to be combined
     *
     * @return A mask where all the individual {@code masks} applied into one mask
     */
    private static int combineMasks(final int... masks) {
        if (masks.length == 0) {
            throw new IllegalArgumentException("The masks cannot be null!");
        }
        int allMasks = masks[0];
        for (int i = 1; i < masks.length; i++) {
            allMasks |= masks[i];
        }
        return allMasks;
    }
}
