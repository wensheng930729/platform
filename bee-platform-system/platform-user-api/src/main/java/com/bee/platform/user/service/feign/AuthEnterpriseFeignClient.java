package com.bee.platform.user.service.feign;

import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthEnterpriseDTO;
import com.bee.platform.user.authority.dto.AuthEnterpriseFeignDetailDTO;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.authority.dto.AuthPlatformUserDto;
import com.bee.platform.user.config.PlatformUserBeatFeignLoggerConfig;
import com.bee.platform.user.hystrix.AuthEnterpriseFeignClientFallbackFactory;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.*;

import java.util.List;


@FeignClient(name = "platform-user", url = "${platform-user.remote-addr}",
        configuration = {PlatformUserBeatFeignLoggerConfig.class},
        fallbackFactory = AuthEnterpriseFeignClientFallbackFactory.class)
public interface AuthEnterpriseFeignClient {

    /**
     * 根据当前登录用户获取-当前用户企业及子企业
     */
    @RequestMapping(method = RequestMethod.GET, value = "/authEnterprise/user/innerflat")
    ResponseResult<List<AuthEnterpriseFlatDTO>> getEnterpriseFlatByUser(@RequestParam("sysToken") String sysToken);

    /**
     * 根据当前登录用户获取-当前用户企业及子企业
     */
    @RequestMapping(method = RequestMethod.GET, value = "/authEnterprise/user/innerflatById")
    ResponseResult<List<AuthEnterpriseFlatDTO>> getEnterpriseFlatByCompanyId(@RequestParam("companyId") Integer companyId);

    /**
     * 根据用户--查询企业及其父子企业
     */
    @RequestMapping(method = RequestMethod.GET, value = "/authEnterprise/parentSubEnterprise")
    ResponseResult<List<AuthEnterpriseFlatDTO>> getParentSubEnterpriseFlat(@RequestParam("sysToken") String sysToken);

    /**
     * 查看单个企业详情
     */
    @RequestMapping(method = RequestMethod.GET, value = "/authEnterprise/{enterpriseId}")
    ResponseResult getEnterpriseDetail(@RequestParam("sysToken") String sysToken, @PathVariable("enterpriseId") Integer enterpriseId);

    /**
     * 根据当前企业id查询
     */
    @RequestMapping(method = RequestMethod.GET, value = "/authEnterprise/enterpriseFeignId")
    ResponseResult<AuthEnterpriseDTO> getParentEnterpriseId(@RequestParam("enterpriseId") Integer enterpriseId);

    /**
     * 当前用户所在企业及-所在集团
     */
    @RequestMapping(method = RequestMethod.GET, value = "/authEnterprise/ancestor/{enterpriseId}")
    ResponseResult<AuthEnterpriseFlatDTO> getAncestor(@PathVariable("enterpriseId") Integer enterpriseId);

    /**
     * 当前用户所在企业及-所在集团
     */
    @RequestMapping(method = RequestMethod.GET, value = "/authEnterprise/userEnterprises/allUsers/{userId}")
    ResponseResult<List<AuthPlatformUserDto>> getEnterpriseUsers(@PathVariable("userId") Integer userId);

    /**
     * 查询多个企业详情
     */
    @RequestMapping(method = RequestMethod.POST, value = "/authEnterprise/moreDetail")
    ResponseResult<List<AuthEnterpriseFeignDetailDTO>> getEnterpriseMoreDetail(@RequestBody List<Integer> orgIds);
}