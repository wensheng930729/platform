package com.bee.platform.dinas.datadriver;

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
 * @description: 进销存系统-砂石
 * @author: zhigang.zhou
 * @create: 2019-08-13 11:21
 **/
@SpringBootApplication
@EnableCircuitBreaker//对hystrixR熔断机制的支持
@EnableAutoConfiguration
@EnableTransactionManagement
@EnableFeignClients(basePackages= {
        FeignPackagesConstant.USER_FEIGN_BASEPACKAGE,
        FeignPackagesConstant.DINASDATADRIVER_FEIGN_BASEPACKAGE
        })
@ComponentScan({BaseScanPackagesConstant.COMMON_BASEPACKAGE,
        BaseScanPackagesConstant.DINASDATADRIVER_BASEPACKAGE,
        BaseScanPackagesConstant.USER_BASEPACKAGE
})
@MapperScan(basePackages = {MapperScanPackagesConstant.USER_MAPPER_BASEPACKAGE,
        MapperScanPackagesConstant.COMMON_MAPPER_BASEPACKAGE,
        MapperScanPackagesConstant.DINASDATADRIVER_MAPPER_BASEPACKAGE
})
public class Application {

    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
