package no.kh498.util;

import java.lang.reflect.Field;

/**
 * @author karl henrik
 * @since 0.1.0
 */
public class Reflection {

    /**
     * @param object
     *     The object to get the field from
     * @param field
     *     The field to get the object of
     *
     * @return The object with the name {@code field} from the object {@code object}
     *
     * @throws NoSuchFieldException
     * @throws IllegalAccessException
     */
    public static Object getField(final Object object, final String field)
    throws NoSuchFieldException, IllegalAccessException {
        final Class<?> c = object.getClass();
        final Field objectField = c.getDeclaredField(field);
        objectField.setAccessible(true);
        final Object result = objectField.get(object);
        objectField.setAccessible(false);
        return result;
    }

    /**
     * Modify an inaccessible field in
     *
     * @param object
     *     The object to get the field from
     * @param field
     *     The field to get the object of
     * @param newValue
     *     The new value of the field {@code field} in the object {@code object}
     *
     * @throws NoSuchFieldException
     * @throws IllegalAccessException
     */
    public static void modifyField(final Object object, final String field, Object newValue)
    throws NoSuchFieldException, IllegalAccessException {
        final Class<?> c = object.getClass();
        final Field objectField = c.getDeclaredField(field);
        objectField.setAccessible(true);
        objectField.set(object, newValue);
        objectField.setAccessible(false);
    }

    /**
     * Modify an inaccessible field in
     *
     * @param object
     *     The object to get the field from
     * @param field
     *     The field to get the object of
     * @param newValue
     *     The new value of the field {@code field} in the object {@code object}
     *
     * @throws NoSuchFieldException
     * @throws IllegalAccessException
     */
    public static void modifySuperField(final Object object, final String field, Object newValue)
    throws NoSuchFieldException, IllegalAccessException {
        final Class<?> c = object.getClass().getSuperclass();
        final Field objectField = c.getDeclaredField(field);
        objectField.setAccessible(true);
        objectField.set(object, newValue);
        objectField.setAccessible(false);
    }
}
