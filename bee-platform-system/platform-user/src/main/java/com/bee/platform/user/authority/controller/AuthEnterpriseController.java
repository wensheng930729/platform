package com.bee.platform.user.authority.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.user.authority.dto.*;
import com.bee.platform.user.authority.entity.AuthEnterprise;
import com.bee.platform.user.authority.rq.*;
import com.bee.platform.user.authority.service.AuthEnterpriseService;
import com.bee.platform.user.authority.service.AuthPlatformUserEnterpriseService;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.EnterpriseDetailDTO;
import com.bee.platform.user.dto.EnterpriseInfoDTO;
import com.bee.platform.user.dto.EnterpriseListDTO;
import com.bee.platform.user.service.EnterprisesCheckService;
import com.bee.platform.user.utils.ValidateUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 企业表 前端控制器
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
@Slf4j
@RestController
@RequestMapping("/authEnterprise")
@Api(value = "新权限企业相关接口", tags = "新权限：企业相关接口")
public class AuthEnterpriseController {

    @Autowired
    private AuthEnterpriseService enterpriseService;
    @Autowired
    private AuthPlatformUserService userService;
    @Autowired
    private EnterprisesCheckService enterprisesCheckService;
    @Autowired
    private AuthPlatformUserEnterpriseService userEnterpriseService;


    @ApiOperation(value = "添加企业")
    @PostMapping(value = "/add")
    public ResponseResult<ResCodeEnum> add(HttpServletRequest request, @RequestBody @Validated AuthEnterpriseAddRQ rq) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (!ValidateUtils.isPhone(rq.getLinkman())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
        }
        return enterpriseService.addEnterprise(rq, userInfo);
    }

    @ApiOperation(value = "删除企业")
    @PostMapping(value = "/del")
    public ResponseResult delEnterprise(HttpServletRequest request, @RequestBody AuthEnterpriseDelRQ rq) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return enterpriseService.delEnterprise(rq, userInfo);
    }

    @ApiOperation(value = "修改企业")
    @PostMapping(value = "/update")
    public ResponseResult<ResCodeEnum> updateEnterprise(HttpServletRequest request, @RequestBody AuthEnterpriseUpdateRQ rq) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return enterpriseService.updateEnterprise(rq, userInfo);
    }

    @ApiOperation(value = "根据条件查询企业-中台")
    @PostMapping(value = "/conditional")
    public ResponseResult<List<AuthEnterpriseListDTO>> getByConditional(HttpServletRequest request, @RequestBody(required = false) AuthEnterpriseGetRQ rq, Page page) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return enterpriseService.getByConditional(rq, userInfo, page);
    }

    @ApiOperation(value = "根据条件查询企业-后台")
    @PostMapping(value = "/conditional/all")
    public ResponseResult<List<AuthEnterpriseListDTO>> getAllByConditional(HttpServletRequest request, @RequestBody(required = false) AuthEnterpriseAllGetRQ rq, Page page) {
        return enterpriseService.getAllByConditional(rq, page);
    }

    @ApiOperation(value = "查看单个企业详情")
    @GetMapping(value = "/{enterpriseId}")
    public ResponseResult<AuthEnterpriseDetailDTO> getEnterpriseDetail(HttpServletRequest request, @PathVariable("enterpriseId") Integer enterpriseId) {
        return enterpriseService.getEnterpriseDetail(enterpriseId);
    }

    @ApiOperation(value = "查看多个企业详情-feign使用")
    @PostMapping(value = "/moreDetail")
    public ResponseResult<List<AuthEnterpriseFeignDetailDTO>> getEnterpriseMoreDetail(@RequestBody List<Integer> orgIds) {
        return enterpriseService.getEnterpriseMoreDetail(orgIds);
    }

    @ApiOperation(value = "查询所有企业信息")
    @GetMapping(value = "/all")
    public ResponseResult<List<AuthEnterpriseAllDTO>> getAllEnterprise() {
        return enterpriseService.getAllEnterprise();
    }

    @ApiOperation(value = "查询所有企业信息--树形结构")
    @GetMapping(value = "/all/tree")
    ResponseResult<List<AuthEnterpriseTreeDTO>> getEnterpriseTree(HttpServletRequest request) {
        return enterpriseService.getEnterpriseTree();
    }

    @ApiOperation(value = "根据用户查询 用户企业树形结构 下拉列表使用 中台")
    @GetMapping(value = "/user/tree")
    public ResponseResult<AuthEnterpriseTreeDTO> getEnterpriseTreeByUser(String sysToken, HttpServletRequest request) {
        if (StringUtils.isEmpty(sysToken)) {
            sysToken = request.getHeader("sysToken");
        }
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return enterpriseService.getEnterpriseTreeByUser(userInfo);
    }

    @ApiOperation(value = "当前用户所在企业及-子企业-下拉列表使用 ")
    @GetMapping(value = "/user/flat")
    public ResponseResult<List<AuthEnterpriseFlatDTO>> getEnterpriseFlatByUser(String sysToken, HttpServletRequest request) {
        if (StringUtils.isEmpty(sysToken)) {
            sysToken = request.getHeader("sysToken");
        }
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return enterpriseService.getEnterpriseFlatByUser(userInfo);
    }

    @ApiOperation(value = "当前用户所在企业及-子企业-下拉列表使用 ")
    @GetMapping(value = "/user/innerflat")
    public ResponseResult<List<AuthEnterpriseFlatDTO>> getEnterpriseFlatByUser(@RequestParam(value = "sysToken") String sysToken) {

        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return enterpriseService.getEnterpriseFlatByUser(userInfo);
    }

    @ApiOperation(value = "根据企业id查询企业及子企业")
    @GetMapping(value = "/user/innerflatById")
    public ResponseResult<List<AuthEnterpriseFlatDTO>> getEnterpriseFlatByCompanyId(@RequestParam(value = "companyId") Integer companyId) {
        if (companyId == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return enterpriseService.getEnterpriseFlatByCompanyId(companyId);
    }

    @ApiOperation(value = "当前用户所在企业及-父子企业 ")
    @GetMapping(value = "/parentSubEnterprise")
    public ResponseResult<List<AuthEnterpriseFlatDTO>> getParentSubEnterpriseFlat(String sysToken, HttpServletRequest request) {
        if (StringUtils.isEmpty(sysToken)) {
            sysToken = request.getHeader("sysToken");
        }
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return enterpriseService.getParentSubEnterpriseFlat(userInfo);
    }

    @ApiOperation(value = "当前用户所在企业及-所在集团 ")
    @GetMapping(value = "/ancestor/{enterpriseId}")
    public ResponseResult<AuthEnterpriseFlatDTO> getAncestor(@PathVariable("enterpriseId") Integer enterpriseId) {
        AuthEnterprise parent = enterpriseService.getAncestor(enterpriseId);
        AuthEnterpriseFlatDTO enterpriseFlatDTO = new AuthEnterpriseFlatDTO().setValue(parent.getId())
                .setLabel(parent.getName());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, enterpriseFlatDTO);
    }

    @ApiOperation(value = "查询企业历史详情(后台)")
    @GetMapping(value = "/history/{enterpriseId}")
    public ResponseResult<AuthEnterpriseHistoryDTO> getEnterpriseHistory(@PathVariable("enterpriseId") Integer enterpriseId) {
        return enterpriseService.getEnterpriseHistory(enterpriseId, new AuthPlatformUserInfo());
    }

    @ApiOperation(value = "根据当前企业id查询")
    @GetMapping(value = "/enterpriseFeignId")
    public ResponseResult<AuthEnterpriseDTO> getParentEnterpriseId(@RequestParam("enterpriseId") Integer enterpriseId) {
        return enterpriseService.getParentEnterpriseId(enterpriseId);
    }

    @ApiOperation(value = "根据用户id查询-用户所在企业及子企业")
    @GetMapping(value = "/userEnterprises/{userId}")
    public ResponseResult<List<EnterpriseListDTO>> getUserEnterprises(@PathVariable("userId") Integer userId) {
        return enterpriseService.getUserEnterprises(userId);
    }

    @ApiOperation(value = "根据用户id查询-用户所在企业及子企业的所有用户")
    @GetMapping(value = "/userEnterprises/allUsers/{userId}")
    public ResponseResult<List<AuthPlatformUserDto>> getEnterpriseUsers(@PathVariable("userId") Integer userId) {
        return enterpriseService.getEnterpriseUsers(userId);
    }

    /**
     * -----------------------TODO 以前的接口 ---------------------------
     */

    @ApiOperation(value = "获取企业信息")
    @RequestMapping(value = "/info", method = RequestMethod.GET)
    public ResponseResult<EnterpriseDetailDTO> getEnterpriseInfo(HttpServletRequest request) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        Integer orgId = userInfo.getOrgId();
        if (userEnterpriseService.findByUserIdAndEnterpriseId(userInfo.getId(), orgId) == null) {
            return ResponseResult.fail("当前用户不属于该部门");
        }
        return enterpriseService.getById(orgId);
    }

    @ApiOperation(value = "企业注册")
    @RequestMapping(value = "/register", method = RequestMethod.POST)
    public ResponseResult register(HttpServletRequest request, @RequestBody() @Valid EnterpriseRegisterInfoRQ rq) {
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("获取用户信息失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return enterprisesCheckService.register(userInfo, rq);
    }

    @ApiOperation(value = "查询是否是已注册企业")
    @RequestMapping(value = "/registeredEnterprise", method = RequestMethod.GET)
    public ResponseResult<List<EnterpriseInfoDTO>> getRegisteredEnterprise(HttpServletRequest request, @RequestParam String enterpriseName) {
        return enterpriseService.listRegisteredEnterpriseInfo(enterpriseName);
    }

    @ApiOperation(value = "企业申请名称校验")
    @GetMapping("/enterpriseCheck")
    public ResponseResult enterpriseCheck(HttpServletRequest request, @RequestParam() String name) {
        String sysToken = request.getHeader("sysToken");
        if (StringUtils.isEmpty(name)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("获取用户信息失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return enterpriseService.enterpriseCheck(userInfo, name);
    }

    @ApiOperation(value = "获取用户的企业列表")
    @RequestMapping(method = RequestMethod.GET)
    public ResponseResult<List<EnterpriseListDTO>> getEnterpriseList(@RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        return enterpriseService.getAllEnterprise(userInfo);
    }

    @ApiOperation(value = "修改企业头像")
    @RequestMapping(value = "/modifyHead", method = RequestMethod.POST)
    public ResponseResult<String> modifyEnterpriseHead(HttpServletRequest request, @RequestParam String head) {
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
        if (org.apache.commons.lang3.StringUtils.isBlank(head)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_HEAD_URL);
        }
        return enterpriseService.modifyEnterpriseHead(head, userInfo);
    }

    @ApiOperation(value = "模糊查询企业列表")
    @GetMapping("/searchEnterpriseList")
    public ResponseResult searchEnterpriseList(@RequestParam() String name, Page page) {
        if (StringUtils.isEmpty(name)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return enterpriseService.searchEnterpriseList(name, page);
    }

}

