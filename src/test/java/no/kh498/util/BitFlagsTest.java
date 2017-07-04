package no.kh498.util;

import org.junit.Assert;
import org.junit.Test;

/**
 * @author kh498
 */
public class BitFlagsTest {
    private static final int OPTION_0 = 0x0; // 0000
    private static final int OPTION_1 = 0x1; // 0001
    private static final int OPTION_2 = 0x2; // 0010

    @Test
    public void testSingle() throws Exception {
        final boolean testTrue = BitFlags.test(OPTION_1, OPTION_1);
        final boolean testFalse = BitFlags.test(OPTION_1, OPTION_2);
        Assert.assertTrue(testTrue);
        Assert.assertFalse(testFalse);
    }

    @Test
    public void testOnly() throws Exception {
        final boolean test = BitFlags.testOnly(OPTION_1, OPTION_1);
        Assert.assertTrue(test);

        final boolean test1 = BitFlags.testOnly(OPTION_1, OPTION_2);
        Assert.assertFalse(test1);
    }

    @Test
    public void setSingle() throws Exception {
        final int result = BitFlags.set(OPTION_0, OPTION_1);
        Assert.assertEquals(OPTION_1, result);
    }
    @Test
    public void setMultiple() throws Exception {
        final int result = BitFlags.set(OPTION_0, OPTION_1, OPTION_2);
        Assert.assertEquals(0x3, result); // (OPTION_1 = 0x1) + (OPTION_2 = 0x2) = 0x3
    }
    @Test
    public void resetOne() throws Exception {
        final int result = BitFlags.reset(OPTION_1, OPTION_1);
        Assert.assertEquals(0x0, result);//0x0 = No flags on
    }
    @Test
    public void resetMultiple() throws Exception {
        final int flagAll = 0x3; // (OPTION_1 = 0x1) + (OPTION_2 = 0x2) = 0x3
        final int flagOne = 0x3; // (OPTION_1 = 0x1) + (OPTION_2 = 0x2) = 0x3
        final int resultResetAll = BitFlags.reset(flagAll, OPTION_1, OPTION_2);
        final int resultResetOne = BitFlags.reset(flagOne, OPTION_2);
        Assert.assertEquals(0x0, resultResetAll);
        Assert.assertEquals(OPTION_1, resultResetOne);
    }
    @Test
    public void flipOne() throws Exception {
        final int resultFlippingOff = BitFlags.flip(OPTION_1, OPTION_1);
        Assert.assertEquals(0x0, resultFlippingOff);//0x0 = No flags on

        final int resultFlippingOn = BitFlags.flip(OPTION_0, OPTION_1);
        Assert.assertEquals(OPTION_1, resultFlippingOn);
    }
    @Test
    public void flipMultiple() throws Exception {
        //Flip off both OPTION_1 and OPTION_2 flag
        final int flagOnAll = 0x3; // (OPTION_1 = 0x1) + (OPTION_2 = 0x2) = 0x3
        final int resultFlippingOffAll = BitFlags.flip(flagOnAll, OPTION_1, OPTION_2);
        Assert.assertEquals(0x0, resultFlippingOffAll); //0x0 = No flags on

        //Flip on both OPTION_1 and OPTION_2 flag
        final int flagOffAll = 0x0; //0x0 = No flags on
        final int resultFlippingOnAll = BitFlags.flip(flagOffAll, OPTION_1, OPTION_2);
        Assert.assertEquals(0x3, resultFlippingOnAll); // (OPTION_1 = 0x1) + (OPTION_2 = 0x2) = 0x3

        //Flip off OPTION_1, leaving OPTION_2 on
        final int flagOnSome = 0x3; // (OPTION_1 = 0x1) + (OPTION_2 = 0x2) = 0x3
        final int resultFlippingOffSome = BitFlags.flip(flagOnSome, OPTION_1);
        Assert.assertEquals(OPTION_2, resultFlippingOffSome); // Bitwise.PLAYERS = 0x2

        //Flip on OPTION_1
        final int resultFlippingOnSome = BitFlags.flip(OPTION_0, OPTION_1);
        Assert.assertEquals(OPTION_1, resultFlippingOnSome); // OPTION_1 = 0x1
    }
}