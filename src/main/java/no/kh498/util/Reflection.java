package no.kh498.util;

import java.lang.reflect.Field;

/**
 * @author karl henrik
 * @since 0.1.0
 */
@SuppressWarnings("unused")
public class Reflection {

    /**
     * Get object from superclass
     *
     * @param object
     *     The object to get the field from
     * @param field
     *     The field to get the object of
     *
     * @return The object with the name {@code field} from the object {@code object}
     *
     * @throws NoSuchFieldException For same reason as {@link Field#get(Object)}
     * @throws IllegalAccessException For same reason as {@link Field#get(Object)}
     */
    public static Object getSuperField(final Object object, final String field)
    throws NoSuchFieldException, IllegalAccessException {
        final Class<?> c = object.getClass().getSuperclass();
        final Field objectField = c.getDeclaredField(field);
        objectField.setAccessible(true);
        final Object result = objectField.get(object);
        objectField.setAccessible(false);
        return result;
    }

    /**
     * @param object
     *     The object to get the field from
     * @param field
     *     The field to get the object of
     *
     * @return The object with the name {@code field} from the object {@code object}
     *
     * @throws NoSuchFieldException For same reason as {@link Field#get(Object)}
     * @throws IllegalAccessException For same reason as {@link Field#get(Object)}
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
     * Modify an inaccessible field
     *
     * @param object
     *     The object to get the field from
     * @param field
     *     The field to get the object of
     * @param newValue
     *     The new value of the field {@code field} in the object {@code object}
     *
     * @throws NoSuchFieldException For same reason as {@link Field#get(Object)}
     * @throws IllegalAccessException For same reason as {@link Field#get(Object)}
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
     * Modify an inaccessible field of the super class
     *
     * @param object
     *     The object to get the field from
     * @param field
     *     The field to get the object of
     * @param newValue
     *     The new value of the field {@code field} in the object {@code object}
     *
     * @throws NoSuchFieldException For same reason as {@link Field#get(Object)}
     * @throws IllegalAccessException For same reason as {@link Field#get(Object)}
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
