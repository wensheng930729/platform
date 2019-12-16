package com.bee.platform.user.config;

import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.web.filter.CorsFilter;


/**
 * @ClassName CorsConfig
 * @Description 解决跨域问题
 * @author zhigang.zhou
 * @Date 2018年10月26日 下午1:16:35
 * @version 1.0.0
 */
@Configuration
public class CorsConfig {
	
	@Bean
	public FilterRegistrationBean<CorsFilter>  corsFilter() {
	    UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
	    CorsConfiguration config = new CorsConfiguration();
	    config.setAllowCredentials(true);   
	    config.addAllowedOrigin("*");
	    config.addAllowedOrigin("null");
	    config.addAllowedHeader("*");
	    config.addAllowedMethod("*");
	    // CORS 配置对所有接口都有效
	    source.registerCorsConfiguration("/**", config);
	    FilterRegistrationBean<CorsFilter> bean = new FilterRegistrationBean<>(new CorsFilter(source));
	    bean.setOrder(0);
	    return bean;
	}
}
