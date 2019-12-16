package com.bee.platform.user.service.feign;

import java.util.List;
import java.util.Set;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.config.PlatformUserBeatFeignLoggerConfig;
import com.bee.platform.user.hystrix.UserInfoFeignClientFallbackFactory;


@FeignClient(name="platform-user",url="${platform-user.remote-addr}", configuration= {PlatformUserBeatFeignLoggerConfig.class}, fallbackFactory= UserInfoFeignClientFallbackFactory.class)
public interface AuthPlatformUserEnterpriseFeignClient {
	 
    @GetMapping("/authPlatformUserEnterprise/findDepartmentIdAndEnterpriseId")
    public ResponseResult<Set<Integer>> findDepartmentIdAndEnterpriseId(@RequestParam List<Integer> departmentId, @RequestParam Integer enterpriseId);
}
