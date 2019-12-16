package com.bee.platform.common.exception;


/**
 * 公共权限 权限禁止访问异常类
 * @author Raphael.dq
 * @date 2019/05/27
 */
public class CommonAuthorityForbiddenException extends RuntimeException {


    /**
     *
     */
    private static final long serialVersionUID = 1L;


    public CommonAuthorityForbiddenException() {
        super();
    }
    
    public CommonAuthorityForbiddenException(String message) {
        super(message);
    }
    
    public CommonAuthorityForbiddenException(String message, Throwable cause) {
        super(message, cause);
    }
}
