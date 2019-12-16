package com.bee.platform.common.utils;

import org.apache.commons.lang3.StringUtils;

/**
 * @Classname StringUtil
 * @Description 字符串处理工具类
 * @Date 2019/5/7 16:22
 * @Author xin.huang
 */
public class StringUtil {

    private static final String DEFAULT = "-";

    private StringUtil() {
    }

    /**
     * @Description 截取邮箱显示
     * @Param email
     * @Author xin.huang
     * @Date 17:09 2019/5/7
     */
    public static String getSpliceEmail(String email) {
        if (StringUtils.isBlank(email)) {
            return "";
        }
        String start = email.substring(0, email.indexOf("@"));
        String end = email.substring(email.indexOf("@"));
        String identifier = "***";
        if (start.length() > 2) {
            start = start.substring(0, 2);
        }
        StringBuffer str = new StringBuffer();
        str.append(start).append(identifier).append(end);
        return str.toString();
    }

    /**
     * @Description 按指定长度截取字符串
     * @Param length
     * @Return
     * @Date 2019/5/13 15:05
     * @Author xin.huang
     */
    public static String splice(String str, int length) {
        if (StringUtils.isBlank(str)) {
            return "";
        }
        if (str.length() > length) {
            return str.substring(0, length);
        }
        return str;
    }

}
