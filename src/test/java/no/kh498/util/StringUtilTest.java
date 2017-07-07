package no.kh498.util;

import org.junit.Assert;
import org.junit.Test;

/**
 * @author kh498
 */
public class StringUtilTest {
    @Test
    public void beautify() throws Exception {
        Assert.assertEquals(StringUtil.beautify(10000), "10 000");
        Assert.assertEquals(StringUtil.beautify(100), "100");
        Assert.assertEquals(StringUtil.beautify(374298763), "374 298 763");
    }
    @Test
    public void ending() throws Exception {
        Assert.assertEquals(StringUtil.ending("Kim"), "Kim's");
        Assert.assertEquals(StringUtil.ending("Mads"), "Mads'");
    }
    @Test
    public void equalsIgnoreCase() throws Exception {
        Assert.assertTrue(StringUtil.equalsIgnoreCase('c', 'c'));
        Assert.assertTrue(StringUtil.equalsIgnoreCase('c', 'C'));

        Assert.assertFalse(StringUtil.equalsIgnoreCase('h', 'i'));
        Assert.assertFalse(StringUtil.equalsIgnoreCase('h', 'I'));
    }
}