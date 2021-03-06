package com.bee.platform.business.service.feign;

import com.bee.platform.business.config.PlatformBusinessUserBeatFeignLoggerConfig;
import com.bee.platform.business.hystrix.BusinessBeatFeignClientFallbackFactory;
import com.bee.platform.common.entity.ResponseResult;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;


@FeignClient(name="platform-business-beat",url="${platform-business.remote-addr}", configuration= {PlatformBusinessUserBeatFeignLoggerConfig.class}, fallbackFactory= BusinessBeatFeignClientFallbackFactory.class)
public interface BusinessBeatFeignClient {

    @RequestMapping(method = RequestMethod.GET, value = "/platform-business/beat")
    ResponseResult<String> beat();
    
}
