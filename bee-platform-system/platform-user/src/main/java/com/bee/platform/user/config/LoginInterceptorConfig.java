 package com.bee.platform.user.config;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import com.bee.platform.user.config.properties.LoginInterceptorConfigProperties;
import com.bee.platform.user.interceptor.LoginInterceptor;
import com.google.common.collect.Lists;


/**
 * @author Raphael.dq
 * @date 2019/05/21
 */
//@Configuration
public class LoginInterceptorConfig implements WebMvcConfigurer {
    
    @Autowired
    LoginInterceptorConfigProperties properties;
    
    @Bean
    public LoginInterceptor loginInterceptor() {
        return new LoginInterceptor();
    }
    
     @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(loginInterceptor()).addPathPatterns("/**").excludePathPatterns(getExcludePaths());
    }
    
    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
    }
    
    private List<String> getExcludePaths() {
        return Lists.newArrayList("/swagger-resources/**", "/webjars/**", "/v2/**", "/swagger-ui.html/**");
    }
    
    
    
}
