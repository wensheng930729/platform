package com.bee.platform.user.service.feign;

import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.user.authority.dto.AuthResourceDetailDTO;
import com.bee.platform.user.config.PlatformUserBeatFeignLoggerConfig;
import com.bee.platform.user.hystrix.AuthResourcesFeignClientFallbackFactory;
import com.bee.platform.user.hystrix.UserInfoFeignClientFallbackFactory;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * @ClassName AuthResourcesFeignClient
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/6/13$ 13:47$
 * @version 1.0.0
 */

@FeignClient(name="platform-user",url="${platform-user.remote-addr}", configuration= {PlatformUserBeatFeignLoggerConfig.class}, fallbackFactory= AuthResourcesFeignClientFallbackFactory.class)
public interface AuthResourcesFeignClient {

    @RequestMapping(method = RequestMethod.GET, value = "/authResource/resourcesByUserSubSys")
    ResponseResult<List<AuthResourceDetailDTO>> listResourcesByUser(@RequestParam String subSys, @RequestParam(required = false) String sysToken);
}
