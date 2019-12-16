package com.bee.platform.user.service.feign;

import com.bee.platform.common.entity.EnterprisesUsers;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.config.PlatformUserBeatFeignLoggerConfig;
import com.bee.platform.user.dto.EnterprisesUsersInfoDTO;
import com.bee.platform.user.hystrix.EnterprisesUsersFeiginClientFallbackFactory;

import javax.validation.Valid;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;


@FeignClient(name="platform-user",url="${platform-user.remote-addr}", configuration= {PlatformUserBeatFeignLoggerConfig.class}, fallbackFactory= EnterprisesUsersFeiginClientFallbackFactory.class)
public interface EnterprisesUsersFeiginClient {
    @RequestMapping(value = "/api/enterprisesUsers/addEnterprisesUsers", method = RequestMethod.POST)
    ResponseResult<EnterprisesUsers> add(@RequestParam("sysToken") String sysToken, @RequestBody  @Valid EnterprisesUsersInfoDTO enterprisesUsersInfoDTO);

}
