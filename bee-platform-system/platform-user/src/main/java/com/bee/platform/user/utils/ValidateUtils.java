package com.bee.platform.user.utils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @Description 验证类
 * @author chenxm66777123
 * @Date 2019年5月9日
 * @version 1.0.0
 */
public class ValidateUtils {


    /**
     * @Description 手机号码验证
     * @author chenxm66777123
     * @Date 2019年5月9日
     * @version 1.0.0
     */
    public static boolean isPhone(String phone) {
        String regex = "^((13[0-9])|(14[5,7,9])|(15([0-3]|[5-9]))|(166)|(17[0,1,3,5,6,7,8])|(18[0-9])|(19[8|9]))\\d{8}$";
        if (phone.length() != 11) {
            return false;
        } else {
            Pattern p = Pattern.compile(regex);
            Matcher m = p.matcher(phone);
            boolean isMatch = m.matches();
            if (!isMatch) {
                return false;
            }
            return isMatch;
        }
    }

}
