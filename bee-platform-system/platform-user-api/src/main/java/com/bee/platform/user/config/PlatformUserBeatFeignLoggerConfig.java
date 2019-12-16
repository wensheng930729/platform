package com.bee.platform.user.config;

import feign.Logger;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class PlatformUserBeatFeignLoggerConfig {

    @Bean
    Logger.Level userFeignLoggerLevel() {
        return Logger.Level.FULL;
    }
    
}
