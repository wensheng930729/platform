package com.bee.platform.user.authority.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumRoleType;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.Validator;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.user.authority.dto.*;
import com.bee.platform.user.authority.enums.AccountType;
import com.bee.platform.user.authority.rq.*;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.CompanyDTO;
import com.bee.platform.user.rq.UserAuthValidateRQ;
import com.bee.platform.user.rq.UserInCompanyRQ;
import com.google.common.base.Preconditions;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 前端控制器
 * </p>
 *
 * @author xin.huang
 * @since 2019-05-20
 */
@Slf4j
@RestController
@RequestMapping("/authPlatformUser")
@CrossOrigin(origins = "*")
@Api(value = "新权限：用户操作相关接口", tags = "新权限：用户操作相关接口")
public class AuthPlatformUserController {

    @Autowired
    private AuthPlatformUserService authPlatformUserService;
    @Autowired
    private AuthPlatformUserService userService;

    @ApiOperation(value = "请求注册验证码", notes = "用户注册请求验证码")
    @ApiImplicitParam(name = "phone", value = "手机号", required = true)
    @RequestMapping(value = "/getReisterCode", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> getRegisterValidateCode(String phone) {
        if (!Validator.isMobile(phone)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
        }
        return authPlatformUserService.getRegisterValidateCode(phone);
    }

    @ApiOperation(value = "校验验证码", notes = "验证手机号与短信验证码或邮箱号与邮箱验证码")
    @ApiImplicitParams({@ApiImplicitParam(name = "account", value = "手机号或邮箱", required = true),
            @ApiImplicitParam(name = "code", value = "验证码", required = true),
            @ApiImplicitParam(name = "type", value = "账号类型：0手机号 1邮箱", required = true, dataType = "int")})
    @RequestMapping(value = "/validate", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> validate(String account, String code, Integer type) {
        log.info("校验验证码传入参数account={}，code={}", account, code);
        if (AccountType.phone.getCode().equals(type)) {
            if (!Validator.isMobile(account)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
            }
        } else {
            if (!Validator.isEmail(account)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.EMAIL_ERROR);
            }
        }
        if (StringUtils.isBlank(code)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CODE_NOT_NULL);
        }
        return authPlatformUserService.validateCode(account, code);
    }

    @ApiOperation(value = "内部系统获取用户信息", notes = "内部系统获取用户信息")
    @RequestMapping(value = "/userInfo", method = RequestMethod.GET)
    public ResponseResult<AuthPlatformUserInfo> getUserInfo(String sysToken) {
        if (org.apache.commons.lang.StringUtils.isEmpty(sysToken)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SYS_TOKEN_NOT_NULL);
        }
        AuthPlatformUserInfo userInfo = authPlatformUserService.getUserInfo(sysToken);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, userInfo);
    }

    @ApiOperation(value = "注册", notes = "注册最后一步")
    @RequestMapping(value = "/register", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> register(@Valid RegisterUserRQ rq) {
        log.info("注册最后一步传入参数{}", rq);
        if (!Validator.isMobile(rq.getPhone())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
        }
        //密码长度要求不少于8位，包含数字和字母
        if (!Validator.isPassword(rq.getPassword())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UNQUALIFIED_PASSWORD);
        }
        return authPlatformUserService.register(rq);
    }

    @ApiOperation(value = "用于存在的用户请求验证码", notes = "用户找回密码请求验证码、企业注册绑定管理员请求验证码、转让超级管理员")
    @ApiImplicitParam(name = "phone", value = "手机号", required = true)
    @RequestMapping(value = "/getValidateCodeWithHasAccount", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> requestValidateCodeWithHasAccount(String phone) {
        if (!Validator.isMobile(phone)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
        }
        return authPlatformUserService.sendMessage(phone);
    }

    @ApiOperation(value = "忘记密码", notes = "设置新密码")
    @RequestMapping(value = "/resetPassword", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> resetPassword(String password, String phone) {
        if (!Validator.isMobile(phone)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
        }
        if (!Validator.isPassword(password)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UNQUALIFIED_PASSWORD);
        }
        return authPlatformUserService.resetPassword(password, phone);
    }

    @ApiOperation(value = "登陆后更新密码")
    @RequestMapping(value = "/updatePassword", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> updatePassword(@RequestHeader(value = "sysToken") String sysToken, String new_password) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        if (!Validator.isPassword(new_password)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UNQUALIFIED_PASSWORD);
        }
        return authPlatformUserService.updatePassword(userInfo, new_password);
    }

    @ApiOperation(value = "通过用户id获取用户信息")
    @ApiImplicitParam(name = "id", value = "用户id", required = true, dataType = "int", paramType = "path")
    @RequestMapping(value = "/{id}", method = RequestMethod.GET)
    public ResponseResult<AuthPlatformUserDto> getUserById(@RequestHeader(value = "sysToken") String sysToken, @PathVariable int id) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getUserInfo(sysToken);
        return authPlatformUserService.getUserById(userInfo, id);
    }

    @ApiOperation(value = "获取用户企业信息")
    @RequestMapping(value = "/companies", method = RequestMethod.POST)
    public ResponseResult<CompanyDTO> getUserInfoCompany(@RequestParam String username, @RequestParam String company) {
        if (StringUtils.isEmpty(username) || StringUtils.isEmpty(company)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        return authPlatformUserService.getUserInfoCompany(username, company);
    }

    @ApiOperation(value = "供应链金融平台查询用户所在企业的id列表")
    @RequestMapping(value = "/company/ids", method = RequestMethod.POST)
    public ResponseResult<List<Integer>> isUserInCompany(@RequestBody @Valid UserInCompanyRQ rq) {
        log.info("供应链金融平台查询用户所在企业的id列表传入的参数是：{}", rq);
        List<Integer> companyIds = authPlatformUserService.userInCompany(rq);
        if (CollectionUtils.isEmpty(companyIds)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, companyIds);
    }

    @ApiOperation(value = "切换企业")
    @ApiImplicitParam(name = "orgId", value = "企业id", required = true, dataType = "int", paramType = "path")
    @RequestMapping(value = "/enterprise/{orgId}", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> switchOrg(HttpServletRequest request, @PathVariable int orgId) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
        return authPlatformUserService.switchOrg(userInfo, orgId);
    }

    @ApiOperation(value = "获取个人信息")
    @RequestMapping(value = "/getSelfInfo", method = RequestMethod.GET)
    public ResponseResult<AuthPlatformUserInfo> getSelfInfo(HttpServletRequest request) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, userInfo);
    }

    @ApiOperation(value = "查询多个用户详情-feign")
    @PostMapping(value = "/moreDetail")
    public ResponseResult<List<AuthPlatformUserFeignDTO>> getMoreUserInfo(@RequestBody List<Integer> ids) {
        return authPlatformUserService.getMoreUserInfo(ids);
    }

    @ApiOperation(value = "获取简单用户信息")
    @RequestMapping(value = "/simpleUserInfo", method = RequestMethod.GET)
    public ResponseResult<AuthPlatformUserInfo> simpleUserInfo(@RequestParam("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, userInfo);
    }

    @ApiOperation(value = "修改个人信息")
    @RequestMapping(value = "/modifySelfInfo", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> modifySelfInfo(HttpServletRequest request, EditAuthPlatformUserRQ editUserRQ) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
        if (!StringUtils.isBlank(editUserRQ.getEmail()) && !Validator.isEmail(editUserRQ.getEmail())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.EMAIL_ERROR);
        }
        return authPlatformUserService.modifySelfInfo(userInfo, editUserRQ);
    }

    @ApiOperation(value = "通过关键词查询用户")
    @PostMapping
    public ResponseResult<List<AuthPlatformUserInfo>> getUserByKeyWord(String sysToken, @RequestParam(required = false) String keyWord, Page page) {
        log.info("子系统请求的参数是:sysToken={},keyWord={}", sysToken, keyWord);
        //验证该用户的登陆信息
        AuthPlatformUserInfo userInfo = authPlatformUserService.getUserInfo(sysToken);
        Pagination pagination = PageUtils.transFromPage(page);
        //通过关键词查询用户信息
        return authPlatformUserService.getUserByKeyWord(userInfo, keyWord, pagination);
    }

    /**
     * @return
     * @Description 修改手机号或邮箱
     * @Author xin.huang
     * @Param editUserAccountRQ
     **/
    @ApiOperation(value = "修改手机号或邮箱", notes = "修改手机号或邮箱")
    @RequestMapping(value = "/updateAccount", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> updateAccount(@RequestHeader(value = "sysToken") String sysToken,
                                                     @RequestBody @Valid EditUserAccountRQ editUserAccountRQ) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        return authPlatformUserService.updateAccount(sysToken, editUserAccountRQ);
    }

    /**
     * @return
     * @Description 获取邮箱验证码
     * @Author xin.huang
     * @Param phone
     * @Param email
     **/
    @ApiOperation(value = "获取邮箱验证码", notes = "获取邮箱验证码")
    @ApiImplicitParams({@ApiImplicitParam(name = "phone", value = "手机号", required = true),
            @ApiImplicitParam(name = "email", value = "邮箱", required = true)})
    @GetMapping("/getEmailCode")
    public ResponseResult<ResCodeEnum> getEmailCode(String phone, String email) {
        if (!Validator.isMobile(phone)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
        }
        if (!Validator.isEmail(email)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.EMAIL_ERROR);
        }
        return authPlatformUserService.sendEmail(phone, email);
    }

    @ApiOperation(value = "内部服务获取用户信息", notes = "为其他服务提供获取用户信息的方法", hidden = true)
    @PostMapping(value = "/info")
    public ResponseResult<AuthUserDto> getUserInfo(String username, @RequestParam("password") String password) {
        log.info("子系统请求的参数是:username={},password={}", username, password);
        if (org.springframework.util.StringUtils.isEmpty(password)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        AuthPlatformUserInfo userInfo = authPlatformUserService.getUserInfo(password);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new AuthUserDto(userInfo));
    }


    @ApiOperation(value = "后台添加用户", notes = "后台添加用户")
    @RequestMapping(value = "/addAuthPlatformUser", method = RequestMethod.PUT)
    public ResponseResult<ResCodeEnum> addAuthPlatformUser(@RequestHeader(value = "sysToken") String sysToken, @RequestBody @Valid AuthPlatformUserAfterRQ authPlatformUserAfterRQ) throws Exception {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        authPlatformUserService.add(userInfo, authPlatformUserAfterRQ);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @ApiOperation(value = "编辑后台用户", notes = "编辑后台用户")
    @RequestMapping(value = "/updataAuthPlatformUser", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> updataAuthPlatformUser(@RequestHeader(value = "sysToken") String sysToken, @RequestBody AuthPlatformUserAfterUpdateRQ authPlatformUserAfterUpdateRQ) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        return authPlatformUserService.update(userInfo, authPlatformUserAfterUpdateRQ);
    }

    @ApiOperation(value = "后台用户启用禁用", notes = "后台用户启用禁用")
    @RequestMapping(value = "/updataById", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> updataById(@RequestHeader(value = "sysToken") String sysToken, @RequestParam int status, @RequestParam int id) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        authPlatformUserService.updateById(userInfo, status, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @ApiOperation(value = "后台用户查询", notes = "后台用户查询")
    @RequestMapping(value = "/query", method = RequestMethod.POST)
    public ResponseResult<List<AuthPlatformUserDto>> query(@RequestHeader(value = "sysToken") String sysToken, @RequestBody AuthPlatformUserAfterSelectRQ authPlatformUserAfterSelectRQ, Page page) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        Pagination pagination = PageUtils.transFromPage(page);
        return authPlatformUserService.query(userInfo, authPlatformUserAfterSelectRQ, pagination);
    }

    @ApiOperation(value = "查询后台用户-下拉", notes = "查询后台用户-下拉")
    @RequestMapping(value = "/backUser", method = RequestMethod.GET)
    ResponseResult<List<AuthPlatformUserPullDownDto>> getBackUser(@RequestHeader(value = "sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return authPlatformUserService.getBackUser();
    }

    @ApiOperation(value = "中台添加用户", notes = "中台添加用户")
    @RequestMapping(value = "/addIn", method = RequestMethod.PUT)
    public ResponseResult<ResCodeEnum> addIn(@RequestHeader(value = "sysToken") String sysToken, @RequestBody @Valid AuthPlatformUserINRQ authPlatformUserINRQ) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        authPlatformUserService.addIn(userInfo, authPlatformUserINRQ);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @ApiOperation(value = "编辑中台用户", notes = "编辑中台用户")
    @RequestMapping(value = "/updateAuthPlatformUserOneIn", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> updateAuthPlatformUserOneIn(@RequestHeader(value = "sysToken") String sysToken, @RequestBody AuthPlatformUserAfterUpdateRQ authPlatformUserAfterUpdateRQ) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        return authPlatformUserService.updateAuthPlatformUserOneIn(userInfo, authPlatformUserAfterUpdateRQ);
    }


    @ApiOperation(value = "中台用户启用禁用", notes = "中台用户启用禁用")
    @RequestMapping(value = "/updataIn", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> updataIn(@RequestHeader(value = "sysToken") String sysToken, @RequestBody AuthPlatformUserUpdateINRQ authPlatformUserUpdateINRQ) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        return authPlatformUserService.updateIn(userInfo, authPlatformUserUpdateINRQ);
    }


    @ApiOperation(value = "查询中台用户", notes = "查询中台用户")
    @RequestMapping(value = "/queryIn", method = RequestMethod.POST)
    public ResponseResult<List<AuthPlatformUserInDTO>> queryIn(@RequestHeader(value = "sysToken") String sysToken, @RequestBody AuthPlatformUserSelectINRQ authPlatformUserSelectINRQ, Page page) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        Pagination pagination = PageUtils.transFromPage(page);
        List<AuthPlatformUserInDTO> list = authPlatformUserService.queryIn(userInfo, authPlatformUserSelectINRQ, pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list, PageUtils.transToPage(pagination));
    }

    @ApiOperation(value = "验证用户是否有权访问指定接口uri", notes = "验证用户是否有权访问指定接口uri")
    @RequestMapping(value = "/validate/privilege", method = RequestMethod.POST)
    public ResponseResult<Boolean> validate(@RequestBody UserAuthValidateRQ rq) {
        return authPlatformUserService.validateRequest(rq);
    }

    @ApiOperation(value = "编辑用户的时候要返回数据", notes = "编辑用户的时候要返回数据")
    @RequestMapping(value = "/getAuthPlatformUserById", method = RequestMethod.PUT)
    public ResponseResult<AuthPlatformUserDto> getAuthPlatformUserById(@RequestHeader(value = "sysToken") String sysToken, @RequestParam Integer id) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        return authPlatformUserService.getAuthPlatformUserById(userInfo, id);
    }


    @ApiOperation(value = "编辑中台用户的时候要返回数据", notes = "编辑中台用户的时候要返回数据")
    @RequestMapping(value = "/getAuthPlatformUserInById", method = RequestMethod.PUT)
    public ResponseResult<AuthPlatformUserDto> getAuthPlatformUserInById(@RequestHeader(value = "sysToken") String sysToken, @RequestParam Integer id) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        return authPlatformUserService.getAuthPlatformUserInById(userInfo, id);
    }


    @ApiOperation(value = "根据用户账号查询用户所拥有的角色", notes = "根据用户账号查询用户所拥有的角色")
    @RequestMapping(value = "/userRoleInfo", method = RequestMethod.GET)
    public ResponseResult<AuthPlatformUserInfo> getAuthRoles(@RequestParam(required = false) String username,
                                                             @RequestParam(required = false) String sysToken, @RequestParam(required = false) String sysType) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        if (StringUtils.isBlank(username)) {
            username = userInfo.getUsername();
        }
        Preconditions.checkArgument(StringUtils.isNotEmpty(username), "用户登录凭证不能为空");
        return authPlatformUserService.getAuthUserRoles(username, userInfo.getOrgId(), sysType);
    }


    @ApiOperation(value = "查询用户是否是管理员", notes = "查询用户是否是管理员")
    @RequestMapping(value = "/isManager", method = RequestMethod.GET)
    ResponseResult<Integer> isManager(HttpServletRequest request) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        Boolean manager = authPlatformUserService.isManager(userInfo.getOrgId(), userInfo.getId(), EnumRoleType.ENTERPRISE_ADMIN.getCode());
        if (manager) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, 1);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, 0);
    }

    @ApiOperation(value = "查询当前用户公司所有人员-下拉列表", notes = "查询当前用户公司所有人员-下拉列表")
    @GetMapping(value = "/getAllCompanyUserById")
    public ResponseResult<List<AuthPlatformUserPullDownDto>> getAllCompanyUserById(@RequestParam("orgId") Integer orgId) {
        return authPlatformUserService.getAllCompanyUserById(orgId);
    }

}

