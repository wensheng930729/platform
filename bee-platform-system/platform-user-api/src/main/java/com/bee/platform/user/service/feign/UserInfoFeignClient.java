package com.bee.platform.user.service.feign;

import java.util.List;

import com.bee.platform.user.authority.dto.AuthPlatformUserPullDownDto;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.*;

import com.bee.platform.common.dto.PlatformManagersDTO;
import com.bee.platform.common.entity.EnterprisesUsers;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.user.config.PlatformUserBeatFeignLoggerConfig;
import com.bee.platform.user.dto.AuthPlatformUserEnterpriseDTO;
import com.bee.platform.user.hystrix.UserInfoFeignClientFallbackFactory;


@FeignClient(name="platform-user",url="${platform-user.remote-addr}", configuration= {PlatformUserBeatFeignLoggerConfig.class}, fallbackFactory= UserInfoFeignClientFallbackFactory.class)
public interface UserInfoFeignClient {

    @RequestMapping(method = RequestMethod.GET, value = "/api/user/userInfo")
    ResponseResult<UserInfo> getUserInfo(@RequestParam(value = "sysToken") String sysToken);

    @RequestMapping(method = RequestMethod.GET, value = "/api/enterprisesUsers/getEnterpriseUserInfoById")
    ResponseResult<EnterprisesUsers> getEnterpriseUserInfoById(@RequestParam(value = "userId") Integer userId, @RequestParam(value = "enterpriseId") Integer enterpriseId);

    @RequestMapping(method = RequestMethod.GET, value = "/manager/getManageByName")
    ResponseResult<PlatformManagersDTO> getManagerByName(@RequestParam(value = "userName") String userName);

    @RequestMapping(method = RequestMethod.GET, value = "/api/user/resetUserInvited")
    ResponseResult<ResCodeEnum> resetUserInvited();
    
    @RequestMapping(method = RequestMethod.POST, value = "/authPlatformUserEnterprise/qureyEnterpriseUser")
    List<AuthPlatformUserEnterpriseDTO> qureyEnterpriseUser(@RequestParam("userId") int userId);

    @RequestMapping(method = RequestMethod.GET, value = "/manager/getSelfInfo")
    ResponseResult<ManagerInfo> getManagerInfo(@RequestParam(value = "sysToken") String sysToken);
    /**
     *
     * @param orgId
     * @return
     */
    @RequestMapping(method = RequestMethod.GET, value = "/authPlatformUser/getAllCompanyUserById")
    ResponseResult<List<AuthPlatformUserPullDownDto>> getAllCompanyUserById(@RequestParam("orgId")Integer orgId);

}
