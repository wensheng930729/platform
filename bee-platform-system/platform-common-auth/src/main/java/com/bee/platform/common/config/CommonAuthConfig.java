 package com.bee.platform.common.config;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import com.bee.platform.common.config.property.CommonAuthConfigProperties;
import com.bee.platform.common.interceptor.CommonAuthInterceptor;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;


/**
 * @author Raphael.dq
 * @date 2019/05/21
 */
@Configuration
@ConditionalOnProperty(prefix = "bee.common.auth", name = "switch-on", havingValue = "true")
public class CommonAuthConfig implements WebMvcConfigurer {
    
    @Autowired
    CommonAuthConfigProperties properties;
    
    @Bean
    public CommonAuthInterceptor commonAuthInterceptor() {
        return new CommonAuthInterceptor();
    }
    
     @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(commonAuthInterceptor()).addPathPatterns("/**").excludePathPatterns(getExcludePaths());
    }
    
    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
    }
    
    private List<String> getExcludePaths() {
        return Optional.ofNullable(properties.getExcludes())
            .map(s -> Splitter.on(",").trimResults().omitEmptyStrings().splitToList(s))
            .orElse(Lists.newArrayList());
    }
    
    
    
}
