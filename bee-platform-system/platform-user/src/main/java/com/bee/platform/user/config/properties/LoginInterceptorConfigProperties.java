 package com.bee.platform.user.config.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import lombok.Getter;
import lombok.Setter;

@Configuration
@ConfigurationProperties(prefix = "bee.platform.login")
@Getter
@Setter
 public class LoginInterceptorConfigProperties {
    
    private String excludes;

}
