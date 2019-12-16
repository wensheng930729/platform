package com.bee.platform.common.utils;

import org.springframework.util.StringUtils;

import javax.servlet.http.HttpServletRequest;

/**
 * @description:
 * @author: junyang.li
 * @create: 2018-11-29 14:52
 **/
public class AuthWebUtils {
    /**
     * @notes 从reques获得参数
     * @Author junyang.li
     * @Date 15:29 2018/12/2
     **/
    public  static String getParam(String key, HttpServletRequest request){
        if(request==null|| StringUtils.isEmpty(key)){
            return null;
        }
        String t=request.getHeader(key.toUpperCase());
        if(StringUtils.isEmpty(t)){
            t=request.getHeader(key);
            return StringUtils.isEmpty(t)?request.getParameter(key):t;
        }
        return t;
    }

}
