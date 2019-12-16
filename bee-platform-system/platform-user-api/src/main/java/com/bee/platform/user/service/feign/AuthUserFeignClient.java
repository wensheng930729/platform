package com.bee.platform.user.service.feign;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthPlatformUserFeignDTO;
import com.bee.platform.user.config.PlatformUserBeatFeignLoggerConfig;
import com.bee.platform.user.hystrix.AuthUserFeignClientFallbackFactory;
import com.bee.platform.user.rq.UserAuthValidateRQ;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Set;


@FeignClient(name = "platform-user", url = "${platform-user.remote-addr}",
        configuration = {PlatformUserBeatFeignLoggerConfig.class},
        fallbackFactory = AuthUserFeignClientFallbackFactory.class)
public interface AuthUserFeignClient {

    /*********************公共权限开放接口**********************/
    @RequestMapping(method = RequestMethod.GET, value = "/authPlatformUser/simpleUserInfo")
    ResponseResult<AuthPlatformUserInfo> simpleUserInfo(@RequestParam("sysToken") String sysToken);

    @RequestMapping(method = RequestMethod.GET, value = "/authPlatformUser/userInfo")
    ResponseResult<AuthPlatformUserInfo> userRoleInfo(@RequestParam("sysToken") String sysToken);

    @RequestMapping(method = RequestMethod.POST, value = "/authPlatformUser/moreDetail")
    ResponseResult<List<AuthPlatformUserFeignDTO>> getMoreUserInfo(@RequestBody List<Integer> ids);

    /**
     * 验证用户是否有权访问指定接口uri
     *
     * @param rq
     * @return
     */
    @RequestMapping(method = RequestMethod.POST, value = "/authPlatformUser/validate/privilege")
    public ResponseResult<Boolean> validate(@RequestBody UserAuthValidateRQ rq);

    /**
     * 获取用户下属用户id列表
     *
     * @param orgId
     * @param userId
     * @return
     */
    @GetMapping(value = "/api/enterprise/department/subordinates")
    public ResponseResult<Set<Integer>> subordinates(@RequestParam Integer orgId, @RequestParam Integer userId);
}
