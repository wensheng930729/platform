package com.bee.platform.common.exception;


/**
 * 公共权限异常类
 * @author Raphael.dq
 * @date 2019/05/27
 */
public class CommonAuthorityException extends RuntimeException {


    /**
     *
     */
    private static final long serialVersionUID = 1L;


    public CommonAuthorityException() {
        super();
    }
    
    public CommonAuthorityException(String message) {
        super(message);
    }
    
    public CommonAuthorityException(String message, Throwable cause) {
        super(message, cause);
    }
}
