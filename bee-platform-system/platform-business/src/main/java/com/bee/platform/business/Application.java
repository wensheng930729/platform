package com.bee.platform.business;

import com.bee.platform.common.constants.BaseScanPackagesConstant;
import com.bee.platform.common.constants.FeignPackagesConstant;
import com.bee.platform.common.constants.MapperScanPackagesConstant;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.circuitbreaker.EnableCircuitBreaker;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.transaction.annotation.EnableTransactionManagement;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-03-01 11:21
 **/
@SpringBootApplication
@EnableCircuitBreaker//对hystrixR熔断机制的支持
@EnableAutoConfiguration
@EnableTransactionManagement
@EnableFeignClients(basePackages= {FeignPackagesConstant.BUSINESS_FEIGN_BASEPACKAGE,
        FeignPackagesConstant.USER_FEIGN_BASEPACKAGE})
@ComponentScan(basePackages={BaseScanPackagesConstant.BUSINESS_BASEPACKAGE,
        BaseScanPackagesConstant.USER_BASEPACKAGE,
        BaseScanPackagesConstant.COMMON_BASEPACKAGE})
@MapperScan(basePackages = {MapperScanPackagesConstant.USER_MAPPER_BASEPACKAGE,
        MapperScanPackagesConstant.BUSINESS_MAPPER_BASEPACKAGE,
        MapperScanPackagesConstant.COMMON_MAPPER_BASEPACKAGE})
@EnableScheduling
public class Application {

    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
