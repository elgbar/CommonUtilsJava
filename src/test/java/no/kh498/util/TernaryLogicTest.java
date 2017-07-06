package no.kh498.util;

import org.junit.Assert;
import org.junit.Test;

/**
 * @author kh498
 */
public class TernaryLogicTest {

    @Test
    public void not() throws Exception {
        Assert.assertEquals(TernaryLogic.FALSE.not(), TernaryLogic.TRUE);
        Assert.assertEquals(TernaryLogic.TRUE.not(), TernaryLogic.FALSE);
        Assert.assertEquals(TernaryLogic.UNKNOWN.not(), TernaryLogic.UNKNOWN);
    }

    @Test
    public void and() throws Exception {
        final TernaryLogic f = TernaryLogic.FALSE;
        final TernaryLogic t = TernaryLogic.TRUE;
        final TernaryLogic u = TernaryLogic.UNKNOWN;

        Assert.assertEquals(f.and(f), f);
        Assert.assertEquals(t.and(f), f);
        Assert.assertEquals(t.and(t), t);

        Assert.assertEquals(u.and(f), f);
        Assert.assertEquals(u.and(t), u);
        Assert.assertEquals(u.and(u), u);
    }

    @Test
    public void or() throws Exception {
        final TernaryLogic f = TernaryLogic.FALSE;
        final TernaryLogic t = TernaryLogic.TRUE;
        final TernaryLogic u = TernaryLogic.UNKNOWN;

        Assert.assertEquals(f.or(f), f);
        Assert.assertEquals(t.or(f), t);
        Assert.assertEquals(t.or(t), t);

        Assert.assertEquals(u.or(f), u);
        Assert.assertEquals(u.or(t), t);
        Assert.assertEquals(u.or(u), u);
    }

    @Test
    public void getInt() throws Exception {
        Assert.assertEquals(TernaryLogic.get(1), TernaryLogic.TRUE);
        Assert.assertEquals(TernaryLogic.get(0), TernaryLogic.FALSE);
        Assert.assertEquals(TernaryLogic.get(23242), TernaryLogic.UNKNOWN);
        Assert.assertEquals(TernaryLogic.get(-23242), TernaryLogic.UNKNOWN);
    }

    @Test
    public void getFloat() throws Exception {
        Assert.assertEquals(TernaryLogic.get(1f), TernaryLogic.TRUE);
        Assert.assertEquals(TernaryLogic.get(0f), TernaryLogic.FALSE);
        Assert.assertEquals(TernaryLogic.get(23242f), TernaryLogic.UNKNOWN);
        Assert.assertEquals(TernaryLogic.get(-23242f), TernaryLogic.UNKNOWN);
    }

    @Test
    public void getDouble() throws Exception {
        Assert.assertEquals(TernaryLogic.get(1d), TernaryLogic.TRUE);
        Assert.assertEquals(TernaryLogic.get(0d), TernaryLogic.FALSE);
        Assert.assertEquals(TernaryLogic.get(23242d), TernaryLogic.UNKNOWN);
        Assert.assertEquals(TernaryLogic.get(-23242d), TernaryLogic.UNKNOWN);
    }

    @Test
    public void getBoolean() throws Exception {
        Assert.assertEquals(TernaryLogic.get(true), TernaryLogic.TRUE);
        Assert.assertEquals(TernaryLogic.get(false), TernaryLogic.FALSE);
    }

    @Test
    public void getString() throws Exception {
        Assert.assertEquals(TernaryLogic.get("true"), TernaryLogic.TRUE);
        Assert.assertEquals(TernaryLogic.get("FAlsE"), TernaryLogic.FALSE);
        Assert.assertEquals(TernaryLogic.get("tREuE"), TernaryLogic.UNKNOWN);
    }

}