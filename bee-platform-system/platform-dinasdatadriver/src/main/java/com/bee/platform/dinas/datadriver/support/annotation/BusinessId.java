 package com.bee.platform.dinas.datadriver.support.annotation;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.PARAMETER;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

/**
 * 标记拦截的业务id
 * @author Raphael.dq
 * @date 2019/03/14
 */
@Retention(RUNTIME)
@Target({FIELD, PARAMETER})
public @interface BusinessId {
}
