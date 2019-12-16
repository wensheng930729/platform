package com.bee.platform.user.controller;

import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.Validator;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.*;
import com.bee.platform.user.enums.AccountType;
import com.bee.platform.user.rq.*;
import com.bee.platform.user.service.ManageUserService;
import com.bee.platform.user.service.UsersService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.lang.ref.SoftReference;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @description: 用户
 * @author: junyang.li
 * @create: 2019-03-04 09:20
 **/
@Slf4j
@Api(value = "用户-API", description = "用户相关API")
@RestController
@RequestMapping("/api/user")
@CrossOrigin(origins = "*")
public class UserController {

    @Autowired
    private UsersService usersService;
    @Autowired
    private ManageUserService manageUserService;
    @Autowired
    private AuthPlatformUserService authPlatformUserService;

    @ApiOperation(value = "请求注册验证码", notes = "用户注册请求验证码")
    @ApiImplicitParam(name = "phone", value = "手机号", required = true)
    @RequestMapping(value = "/getReisterCode", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> getRegisterValidateCode(String phone) {
        if (!Validator.isMobile(phone)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
        }
        return usersService.getRegisterValidateCode(phone);
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
        return usersService.validateCode(account, code);
    }

    @ApiOperation(value = "内部系统获取用户信息", notes = "内部系统获取用户信息")
    @RequestMapping(value = "/userInfo", method = RequestMethod.GET)
    public ResponseResult<UserInfo> getUserInfo(String sysToken) {
        if (StringUtils.isEmpty(sysToken)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SYS_TOKEN_NOT_NULL);
        }
        UserInfo userInfo = usersService.getUserInfo(sysToken);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, userInfo);
    }


    @ApiOperation(value = "注册", notes = "注册最后一步")
    @RequestMapping(value = "/register", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> register(@Valid RegisterRQ rq) {
        log.info("注册最后一步传入参数{}", rq);
        if (!Validator.isMobile(rq.getPhone())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
        }
        //密码长度要求不少于8位，包含数字和字母
        if (!Validator.isPassword(rq.getPassword())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UNQUALIFIED_PASSWORD);
        }
        return usersService.register(rq);
    }


    @ApiOperation(value = "用于存在的用户请求验证码", notes = "用户找回密码请求验证码、企业注册绑定管理员请求验证码、转让超级管理员")
    @ApiImplicitParam(name = "phone", value = "手机号", required = true)
    @RequestMapping(value = "/getValidateCodeWithHasAccount", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> requestValidateCodeWithHasAccount(String phone) {
        if (!Validator.isMobile(phone)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
        }
        return usersService.sendMessage(phone);
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
        return usersService.resetPassword(password, phone);
    }

    @ApiOperation(value = "登陆后更新密码")
    @RequestMapping(value = "/updatePassword", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> updatePassword(@RequestHeader(value = "sysToken") String sysToken, String new_password) {
        UserInfo userInfo = usersService.getSelfInfo(sysToken);
        if (!Validator.isPassword(new_password)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UNQUALIFIED_PASSWORD);
        }
        return usersService.updatePassword(userInfo, new_password);
    }

    @ApiOperation(value = "通过用户id获取用户信息")
    @ApiImplicitParam(name = "id", value = "用户id", required = true, dataType = "int", paramType = "path")
    @RequestMapping(value = "/{id}", method = RequestMethod.GET)
    public ResponseResult<UserDetailDTO> getUserById(@RequestHeader(value = "sysToken") String sysToken, @PathVariable int id) {
        UserInfo userInfo = usersService.getUserInfo(sysToken);
        return usersService.getUserById(userInfo, id);
    }


    @ApiOperation(value = "获取用户企业信息")
    @RequestMapping(value = "/companies", method = RequestMethod.POST)
    public ResponseResult<CompanyDTO> getUserInfoCompany(@RequestParam String username, @RequestParam String company) {
        if (StringUtils.isEmpty(username) || StringUtils.isEmpty(company)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        return usersService.getUserInfoCompany(username, company);
    }

    @ApiOperation(value = "供应链金融平台查询用户所在企业的id列表")
    @RequestMapping(value = "/company/ids", method = RequestMethod.POST)
    public ResponseResult<List<Integer>> isUserInCompany(@RequestBody @Valid UserInCompanyRQ rq) {
        log.info("供应链金融平台查询用户所在企业的id列表传入的参数是：{}", rq);
        List<Integer> companyIds = usersService.userInCompany(rq);
        if (CollectionUtils.isEmpty(companyIds)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, companyIds);
    }


    @ApiOperation(value = "切换企业")
    @ApiImplicitParam(name = "org_id", value = "企业id", required = true, dataType = "int", paramType = "path")
    @RequestMapping(value = "/enterprise/{org_id}", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> switchOrg(HttpServletRequest request, @PathVariable int org_id) {
        UserInfo userInfo = usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
        return usersService.switchOrg(userInfo, org_id);
    }

    @ApiOperation(value = "获取个人信息")
    @RequestMapping(value = "/getSelfInfo", method = RequestMethod.GET)
    public ResponseResult<UserInfo> getSelfInfo(HttpServletRequest request) {
        UserInfo userInfo = usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, userInfo);
    }

    @ApiOperation(value = "删除员工")
    @ApiImplicitParam(name = "id", value = "用户id", required = true, dataType = "int", paramType = "path")
    @RequestMapping(value = "/{id}", method = RequestMethod.DELETE)
    public ResponseResult<ResCodeEnum> deleteUserForDepartment(HttpServletRequest request, @PathVariable int id) {
        UserInfo userInfo = usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
        return usersService.deleteUserForDepartment(userInfo, id);
    }


    @ApiOperation(value = "修改个人信息")
    @RequestMapping(value = "/modifySelfInfo", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> modifySelfInfo(HttpServletRequest request, EditUserRQ editUserRQ) {
        UserInfo userInfo = usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
        if (!StringUtils.isBlank(editUserRQ.getEmail()) && !Validator.isEmail(editUserRQ.getEmail())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.EMAIL_ERROR);
        }
        return usersService.modifySelfInfo(userInfo, editUserRQ);
    }

    @ApiOperation(value = "获取角色")
    @RequestMapping(value = "/getRole", method = RequestMethod.GET)
    public ResponseResult<Map<String, String>> getRole(HttpServletRequest request) {
        UserInfo userInfo = usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
        Map<String, String> json = new SoftReference<>(new HashMap<String, String>(16)).get();
        json.put("role", userInfo.getRoleName());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, json);
    }

    @ApiOperation(value = "修改头像")
    @RequestMapping(value = "/modify_head", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> modifyHead(HttpServletRequest request, @RequestParam String head) {
        if (StringUtils.isEmpty(head)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_HEAD_URL);
        }
        UserInfo userInfo = usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
        return usersService.modifyHead(userInfo, head);
    }

    @ApiOperation(value = "搜索用户")
    @GetMapping(value = "/find_user_and_department", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseResult<UserBasicDTO> findUserAndDepartment(HttpServletRequest request, @RequestParam String name) {
        log.info("search name:{}", name);

        UserInfo userInfo = usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
        return usersService.findUserAndDepartment(userInfo, name);
    }

    @ApiOperation(value = "通过关键词查询用户")
    @PostMapping
    public ResponseResult<List<UserInfo>> getUserByKeyWord(String sysToken, String keyWord) {
        log.info("子系统请求的参数是:sysToken={},keyWord={}", sysToken, keyWord);
        if (StringUtils.isEmpty(sysToken) || StringUtils.isEmpty(keyWord)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.KEY_WORD_NOT_FOUND);
        }
        //验证该用户的登陆信息
        usersService.getUserInfo(sysToken);
        //通过关键词查询用户信息
        return usersService.getUserByKeyWord(keyWord);
    }

    @ApiOperation(value = "内部服务获取用户信息", notes = "为其他服务提供获取用户信息的方法", hidden = true)
    @PostMapping(value = "/info")
    public ResponseResult<UserDTO> getUserInfo(String username, @RequestParam("password") String password) {
        log.info("子系统请求的参数是:username={},password={}", username, password);
        if (org.springframework.util.StringUtils.isEmpty(password)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
//        UserInfo userInfo = usersService.getUserInfo(password);
        AuthPlatformUserInfo userInfo = authPlatformUserService.getUserInfo(password);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new UserDTO(userInfo));
    }

    /**
     * @notes 每天0点重置用户邀请状态
     * @Author junyang.li
     * @Date 16:22 2019/3/20
     **/
    @ApiOperation(value = "每天0点重置用户邀请状态", notes = "每天0点重置用户邀请状态", hidden = true)
    @GetMapping(value = "/resetUserInvited")
    public ResponseResult<ResCodeEnum> resetUserInvited() {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
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
        UserInfo userInfo = usersService.getSelfInfo(sysToken);
        return usersService.updateAccount(sysToken, editUserAccountRQ);
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
        return usersService.sendEmail(phone, email);
    }

    @ApiOperation(value = "查询用户关联列表", notes = "查询用户关联列表")
    @RequestMapping(value = "/list", method = RequestMethod.POST)
    public ResponseResult<List<UserManagerListDTO>> updateAccount1(@RequestBody UserManagerListRQ rq, Page page) {
        return usersService.getUserManagerList(rq, page);
    }

    @ApiOperation(value = "查询用户关联列表个人详情", notes = "查询用户关联列表个人详情")
    @RequestMapping(value = "/list/{userId}", method = RequestMethod.GET)
    public ResponseResult<UserListDetailDTO> getUserListDetail(@RequestHeader(value = "sysToken") String sysToken, @PathVariable("userId") Integer userId) {
        ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);
        if (ObjectUtils.isEmpty(managerInfo)) {
            log.error("获取管理员信息失败，类:{0} 方法:{1}", "UserController", "getUserListDetail");
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(userId)) {
            log.error("用户id为空 类:{0} 方法:{1}", "UserController", "getUserListDetail");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return usersService.getUserListDetail(userId,managerInfo);
    }

    @ApiOperation(value = "修改用户关联列表个人详情", notes = "修改用户关联列表个人详情")
    @RequestMapping(value = "/list/update", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> updateUserListDetail(@RequestHeader(value = "sysToken") String sysToken, @RequestBody @Validated UserListDetailRQ rq) {
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数不能为空，类:{0} 方法:{1}", "UserController", "getUserListDetail");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);
        if (ObjectUtils.isEmpty(managerInfo)) {
            log.error("获取管理员信息失败，类:{0} 方法:{1}", "UserController", "getUserListDetail");
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return usersService.updateUserListDetail(rq, managerInfo);
    }
}
