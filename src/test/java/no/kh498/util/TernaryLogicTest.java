package no.kh498.util;

import org.junit.Assert;
import org.junit.Test;

/**
 * @author karl henrik
 * @since 0.1.0
 */
public class TernaryLogicTest {
    @Test
    public void not() throws Exception {
        Assert.assertTrue(TernaryLogic.FALSE.not() == TernaryLogic.TRUE);
        Assert.assertTrue(TernaryLogic.TRUE.not() == TernaryLogic.FALSE);
        Assert.assertTrue(TernaryLogic.UNKNOWN.not() == TernaryLogic.UNKNOWN);

    }
    @Test
    public void and() throws Exception {
        final TernaryLogic f = TernaryLogic.FALSE;
        final TernaryLogic t = TernaryLogic.TRUE;
        final TernaryLogic u = TernaryLogic.UNKNOWN;

        Assert.assertTrue(f.and(f) == f);
        Assert.assertTrue(t.and(f) == f);
        Assert.assertTrue(t.and(t) == t);

        Assert.assertTrue(u.and(f) == f);
        Assert.assertTrue(u.and(t) == u);
        Assert.assertTrue(u.and(u) == u);
    }
    
    @Test
    public void or() throws Exception {
        final TernaryLogic f = TernaryLogic.FALSE;
        final TernaryLogic t = TernaryLogic.TRUE;
        final TernaryLogic u = TernaryLogic.UNKNOWN;

        Assert.assertTrue(f.or(f) == f);
        Assert.assertTrue(t.or(f) == t);
        Assert.assertTrue(t.or(t) == t);

        Assert.assertTrue(u.or(f) == u);
        Assert.assertTrue(u.or(t) == t);
        Assert.assertTrue(u.or(u) == u);
    }
    @Test
    public void getInt() throws Exception {
        Assert.assertTrue(TernaryLogic.get(1) == TernaryLogic.TRUE);
        Assert.assertTrue(TernaryLogic.get(0) == TernaryLogic.FALSE);
        Assert.assertTrue(TernaryLogic.get(23242) == TernaryLogic.UNKNOWN);
        Assert.assertTrue(TernaryLogic.get(-23242) == TernaryLogic.UNKNOWN);
    }
    @Test
    public void getFloat() throws Exception {
        Assert.assertTrue(TernaryLogic.get(1f) == TernaryLogic.TRUE);
        Assert.assertTrue(TernaryLogic.get(0f) == TernaryLogic.FALSE);
        Assert.assertTrue(TernaryLogic.get(23242f) == TernaryLogic.UNKNOWN);
        Assert.assertTrue(TernaryLogic.get(-23242f) == TernaryLogic.UNKNOWN);
    }
    @Test
    public void getDouble() throws Exception {
        Assert.assertTrue(TernaryLogic.get(1d) == TernaryLogic.TRUE);
        Assert.assertTrue(TernaryLogic.get(0d) == TernaryLogic.FALSE);
        Assert.assertTrue(TernaryLogic.get(23242d) == TernaryLogic.UNKNOWN);
        Assert.assertTrue(TernaryLogic.get(-23242d) == TernaryLogic.UNKNOWN);
    }
    @Test
    public void getBoolean() throws Exception {
        Assert.assertTrue(TernaryLogic.get(true) == TernaryLogic.TRUE);
        Assert.assertTrue(TernaryLogic.get(false) == TernaryLogic.FALSE);
    }
    @Test
    public void getString() throws Exception {
        Assert.assertTrue(TernaryLogic.get("true") == TernaryLogic.TRUE);
        Assert.assertTrue(TernaryLogic.get("false") == TernaryLogic.FALSE);
        Assert.assertTrue(TernaryLogic.get("tREuE") == TernaryLogic.UNKNOWN);
    }

}