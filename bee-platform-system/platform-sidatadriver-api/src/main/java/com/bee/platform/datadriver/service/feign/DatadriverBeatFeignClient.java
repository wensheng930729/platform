package com.bee.platform.datadriver.service.feign;

import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.config.PlatformDatadriverBeatFeignLoggerConfig;
import com.bee.platform.datadriver.hystrix.DatadriverBeatFeignClientFallbackFactory;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;


@FeignClient(name="platform-sidatadriver-beat",url="${platform-sidatadriver.remote-addr}", configuration= {PlatformDatadriverBeatFeignLoggerConfig.class}, fallbackFactory= DatadriverBeatFeignClientFallbackFactory.class)
public interface DatadriverBeatFeignClient {

    @RequestMapping(method = RequestMethod.GET, value = "/platform-sidatadriver/beat")
    ResponseResult<String> beat();
    
}
