package com.bee.platform.user.service.feign;

import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.config.PlatformUserBeatFeignLoggerConfig;
import com.bee.platform.user.hystrix.UserBeatFeignClientFallbackFactory;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;


@FeignClient(name="platform-user-beat",url="${platform-user.remote-addr}", configuration= {PlatformUserBeatFeignLoggerConfig.class}, fallbackFactory= UserBeatFeignClientFallbackFactory.class)
public interface UserBeatFeignClient {

    @RequestMapping(method = RequestMethod.GET, value = "/beat")
    ResponseResult<String> beat();
    
}
