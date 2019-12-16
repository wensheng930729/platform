 package com.bee.platform.datadriver.support.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.support.OperateType;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface Log {
    
    /**
     * 标识业务类型
     * @return
     */
    EnumBusinessType businessType();
    
    /**
     * 操作类型
     * @return
     */
    OperateType operateType();
    /**
     * 具体操作描述
     * @return
     */
    String msg() default "";
    
    /**
     * 是否为批量操作
     * @return
     */
    boolean isBatch() default false;
    
    
}
