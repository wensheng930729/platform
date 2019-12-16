package com.bee.platform;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.circuitbreaker.EnableCircuitBreaker;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import com.bee.platform.common.constants.BaseScanPackagesConstant;
import com.bee.platform.common.constants.FeignPackagesConstant;
import com.bee.platform.common.constants.MapperScanPackagesConstant;

/**
 * @description:
 * @author: zhigang.zhou
 * @create: 2019-03-01 11:21
 **/
@SpringBootApplication
@EnableCircuitBreaker//对hystrixR熔断机制的支持
@EnableAutoConfiguration
@EnableTransactionManagement
@EnableFeignClients(basePackages= {FeignPackagesConstant.BUSINESS_FEIGN_BASEPACKAGE,
        FeignPackagesConstant.USER_FEIGN_BASEPACKAGE,
        FeignPackagesConstant.DATADRIVER_FEIGN_BASEPACKAGE
        })
@ComponentScan({BaseScanPackagesConstant.COMMON_BASEPACKAGE,
        BaseScanPackagesConstant.DATADRIVER_BASEPACKAGE,
        BaseScanPackagesConstant.USER_BASEPACKAGE,
        BaseScanPackagesConstant.BUSINESS_BASEPACKAGE,
        BaseScanPackagesConstant.CUSTOMER_BASEPACKAGE,
        BaseScanPackagesConstant.COSTCONTROL_BASEPACKAGE
})
@MapperScan(basePackages = {MapperScanPackagesConstant.USER_MAPPER_BASEPACKAGE,
        MapperScanPackagesConstant.BUSINESS_MAPPER_BASEPACKAGE,
        MapperScanPackagesConstant.COMMON_MAPPER_BASEPACKAGE,
        MapperScanPackagesConstant.DATADRIVER_MAPPER_BASEPACKAGE,
        MapperScanPackagesConstant.CUSTOMER_MAPPER_BASEPACKAGE,
        MapperScanPackagesConstant.COSTCONTROL_MAPPER_BASEPACKAGE
})
public class Application {

    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
