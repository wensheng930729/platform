package com.bee.platform.dinas.datadriver.service.feign;

import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.config.PlatformDinasDatadriverBeatFeignLoggerConfig;
import com.bee.platform.dinas.datadriver.hystrix.DinasDatadriverBeatFeignClientFallbackFactory;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;


@FeignClient(name="platform-dinasdatadriver-beat",url="${platform-dinasdatadriver.remote-addr}", configuration= {PlatformDinasDatadriverBeatFeignLoggerConfig.class}, fallbackFactory= DinasDatadriverBeatFeignClientFallbackFactory.class)
public interface DinasDatadriverBeatFeignClient {

    @RequestMapping(method = RequestMethod.GET, value = "/platform-dinasdatadriver/beat")
    ResponseResult<String> beat();
    
}
