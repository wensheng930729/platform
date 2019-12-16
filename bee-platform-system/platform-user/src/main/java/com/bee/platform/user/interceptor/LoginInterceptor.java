package com.bee.platform.user.interceptor;

import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.servlet.HandlerInterceptor;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.utils.AuthConstantsUtil;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.config.properties.LoginInterceptorConfigProperties;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;

/**
 * @author Raphael.dq
 * @date 2019/05/21
 */
@Slf4j
public class LoginInterceptor implements HandlerInterceptor {
    
    @Autowired
    LoginInterceptorConfigProperties configProperties;
    
    @Autowired
    AuthPlatformUserService authPlatformUserService;
    
    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler)
        throws Exception {
        String uri = request.getRequestURI();
        log.info("Intercept uri {}", uri);
        if (exclude(uri)) {
            log.info("exclude uri {}", uri);
            return true;
        }
        // 服务内部调用,放行
        String innerFlag = request.getHeader(AuthConstantsUtil.INNER_CLIENT_ID);
        if (StringUtils.isNotEmpty(innerFlag)) {
            return true;
        }
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        if (StringUtils.isNotBlank(sysToken)) {
            AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
            if (userInfo != null) {
                return true;
            }
        }
        
        log.info("uri {}, cannot find userInfo", uri);
        return false;
        
    }
    
    public boolean exclude(String path) {
        List<String> patterns = getExcludePaths();
        for (String pattern : patterns) {
            if (Pattern.compile(pattern).matcher(path).find()) {
                return true;
            }
        }
        return false;
    }
    
    private List<String> getExcludePaths() {
        return Optional.ofNullable(configProperties.getExcludes())
            .map(s -> Splitter.on(",").trimResults().omitEmptyStrings().splitToList(s))
            .orElse(Lists.newArrayList());
    }
    

}
