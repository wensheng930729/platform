package com.bee.platform.common.utils;

import org.springframework.util.StringUtils;

import java.math.BigDecimal;
import java.text.DecimalFormat;

/**
 * @description:  BigDecimal工具类
 * @author: junyang.li
 * @create: 2019-02-01 15:28
 **/
public class BigDecimalUtils {

    public static final String TWO_DECIMAL_PLACES="##0.00";

    /**
     * @notes 格式化decimal数据
     * @Author junyang.li
     * @Date 15:33 2019/2/1
     **/
    public static String format(String pattern, BigDecimal bigDecimal){
        if(StringUtils.isEmpty(pattern)||bigDecimal==null){
            return null;
        }
        return new DecimalFormat(pattern).format(bigDecimal);
    }
}
