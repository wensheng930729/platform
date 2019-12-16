package com.bee.platform.datadriver.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.web.filter.CorsFilter;


/**
 * @ClassName CorsConfig
 * @Description 解决跨域问题
 * @author zhigang.zhou
 * @Date 2018年11月27日 下午1:16:35
 * @version 1.0.0
 */
@Configuration
public class CorsConfig {
	@Bean
	public CorsFilter corsFilter() {
		final UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
		final CorsConfiguration corsConfiguration = new CorsConfiguration();
		corsConfiguration.addAllowedOrigin("*");
		corsConfiguration.addAllowedHeader("*");
		corsConfiguration.addAllowedMethod("*");
		corsConfiguration.setAllowCredentials(true);
		corsConfiguration.setMaxAge(3600L);
		source.registerCorsConfiguration("/**", corsConfiguration);
		return new CorsFilter(source);
	}
}
