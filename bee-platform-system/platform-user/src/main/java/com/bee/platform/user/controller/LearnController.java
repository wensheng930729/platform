package com.bee.platform.user.controller;


import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumRoleType;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.ConstInfos;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.LearnDTO;
import com.bee.platform.user.service.LearnService;
import com.google.common.collect.Maps;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;

/**
 * <p>
 * 前端控制器
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-03-04
 */
@Slf4j
@Api(value = "学习指南-API", tags = "学习指南相关接口")
@RestController
@RequestMapping("/api/user/learn")
@CrossOrigin(origins = "*")
public class LearnController {

    @Autowired
    private LearnService learnService;

    /*@Autowired
    private UsersService usersService;*/
    @Autowired
    private AuthPlatformUserService userService;


    @ApiOperation(value = "发布学习指南")
    @RequestMapping(method = RequestMethod.POST, consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE)
    public ResponseResult addLearn(HttpServletRequest request,
                                   @RequestParam String title,
                                   @RequestParam String content,
                                   @RequestParam int type,
                                   @RequestParam String depName) {

        log.info("title:{}, content:{}", title, content);
        if (StringUtils.isEmpty(title) || StringUtils.isEmpty(content) || ObjectUtils.isEmpty(type) || StringUtils.isEmpty(depName)) {
            log.error("参数不全");
            throw new BusinessException(ResCodeEnum.PARAMETER_INCOMPLETE, ExceptionMessageEnum.USER_VALUE_EMPTY);
        }
        // 获取用户信息
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader("sysToken"));*/
        AuthPlatformUserInfo userInfo = userService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("获取用户信息失败");
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO,ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        /*if (!RoleType.SUPER.getDesc().equals(userInfo.getRoleName()) &&
                !RoleType.ADMIN.getDesc().equals(userInfo.getRoleName())) {
            log.error("当前用户没有权限");
            throw new BusinessException(ResCodeEnum.NO_AUTHORITY,ExceptionMessageEnum.NO_AUTHORITY);
        }*/
        if (!userService.isManager(userInfo.getId(),userInfo.getOrgId(), EnumRoleType.ENTERPRISE_ADMIN.getCode()) ||
                !userService.isManager(userInfo.getId(),userInfo.getOrgId(),EnumRoleType.SUPER_ADMIN.getCode())) {
            log.info("当前用户没有权限");
            throw new BusinessException(ResCodeEnum.NO_AUTHORITY,ExceptionMessageEnum.NO_AUTHORITY);

        }

        return learnService.addLearn(userInfo, title, content, type, depName);
    }


    @ApiOperation(value = "修改学习指南")
    @RequestMapping(value = "/{id}", method = RequestMethod.POST)
    public ResponseResult modifyLearn(HttpServletRequest request,
                                      @PathVariable int id,
                                      @RequestParam String title,
                                      @RequestParam String content,
                                      @RequestParam int type,
                                      @RequestParam String depName) {

        if (ObjectUtils.isEmpty(id) || StringUtils.isEmpty(title) || StringUtils.isEmpty(content) || ObjectUtils.isEmpty(type) || StringUtils.isEmpty(depName)) {
            log.error("参数不全");
            throw new BusinessException(ResCodeEnum.PARAMETER_INCOMPLETE,ExceptionMessageEnum.USER_VALUE_EMPTY);
        }
        // 获取用户信息
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader("sysToken"));*/
        AuthPlatformUserInfo userInfo = userService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("获取用户信息失败");
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO,ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        /*if (!RoleType.SUPER.getDesc().equals(userInfo.getRoleName()) &&
                !RoleType.ADMIN.getDesc().equals(userInfo.getRoleName())) {
            log.error("当前用户没有权限");
            throw new BusinessException(ResCodeEnum.NO_AUTHORITY,ExceptionMessageEnum.NO_AUTHORITY);
        }*/

        if (!userService.isManager(userInfo.getId(),userInfo.getOrgId(), EnumRoleType.ENTERPRISE_ADMIN.getCode()) ||
                !userService.isManager(userInfo.getId(),userInfo.getOrgId(),EnumRoleType.SUPER_ADMIN.getCode())) {
            log.info("当前用户没有权限");
            throw new BusinessException(ResCodeEnum.NO_AUTHORITY,ExceptionMessageEnum.NO_AUTHORITY);

        }
        return learnService.modifyLearn(userInfo, id, title, content, type, depName);
    }


    @ApiOperation(value = "删除学习指南")
    @RequestMapping(value = "/{id}", method = RequestMethod.DELETE)
    public ResponseResult deleteLearn(HttpServletRequest request, @PathVariable int id) {

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数不全");
            throw new BusinessException(ResCodeEnum.PARAMETER_INCOMPLETE,ExceptionMessageEnum.USER_VALUE_EMPTY);
        }
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader("sysToken"));*/
        AuthPlatformUserInfo userInfo = userService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("获取用户信息失败");
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO,ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        /*if (!RoleType.SUPER.getDesc().equals(userInfo.getRoleName()) &&
                !RoleType.ADMIN.getDesc().equals(userInfo.getRoleName())) {
            log.error("当前用户没有权限");
            throw new BusinessException(ResCodeEnum.NO_AUTHORITY,ExceptionMessageEnum.NO_AUTHORITY);
        }*/

        if (!userService.isManager(userInfo.getId(),userInfo.getOrgId(), EnumRoleType.ENTERPRISE_ADMIN.getCode()) ||
                !userService.isManager(userInfo.getId(),userInfo.getOrgId(),EnumRoleType.SUPER_ADMIN.getCode())) {
            log.info("当前用户没有权限");
            throw new BusinessException(ResCodeEnum.NO_AUTHORITY,ExceptionMessageEnum.NO_AUTHORITY);

        }
        return learnService.deleteLearn(userInfo, id);
    }


    @ApiOperation(value = "根据id获取学习指南")
    @RequestMapping(value = "/getLearnArticlesById/{id}", method = RequestMethod.GET)
    public ResponseResult<LearnDTO> getLearnById(HttpServletRequest request, @PathVariable int id) {

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数不全");
            throw new BusinessException(ResCodeEnum.PARAMETER_INCOMPLETE,ExceptionMessageEnum.USER_VALUE_EMPTY);
        }
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader("sysToken"));*/
        AuthPlatformUserInfo userInfo = userService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("获取用户信息失败");
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO,ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        return learnService.getLearnById(userInfo, id);
    }


    @ApiOperation(value = "按标题搜索学习指南")
    @RequestMapping(value = "/search", method = RequestMethod.GET)
    public ResponseResult<List<LearnDTO>> findLearnByTitle(HttpServletRequest request, @RequestParam String title) {

        if(StringUtils.isEmpty(title)){
            log.error("参数不全");
            throw new BusinessException(ResCodeEnum.PARAMETER_INCOMPLETE,ExceptionMessageEnum.USER_VALUE_EMPTY);
        }
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader("sysToken"));*/
        AuthPlatformUserInfo userInfo = userService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("获取用户信息失败");
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO,ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        return learnService.findLearnByTitle(userInfo, title);
    }

    @ApiOperation(value = "根据类型获取学习指南列表")
    @RequestMapping(value = "/getAllByType", method = RequestMethod.GET)
    public ResponseResult<List<LearnDTO>> getAllByType(HttpServletRequest request, @RequestParam int type, Page page) {

        if(ObjectUtils.isEmpty(type)){
            log.error("参数不全");
            throw new BusinessException(ResCodeEnum.PARAMETER_INCOMPLETE,ExceptionMessageEnum.USER_VALUE_EMPTY);
        }
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader("sysToken"));*/
        AuthPlatformUserInfo userInfo = userService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("获取用户信息失败");
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO,ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        return learnService.getAllByType(userInfo, type, page);
    }

    @ApiOperation(value = "获取学习指南列表")
    @RequestMapping(value = "/getAll", method = RequestMethod.GET)
    public ResponseResult<List<LearnDTO>> getAll(HttpServletRequest request, Page page) {

        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader("sysToken"));*/
        AuthPlatformUserInfo userInfo = userService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("获取用户信息失败");
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO,ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }

        return learnService.getAll(userInfo, page);
    }


    @ApiOperation(value = "获取学习指南列表")
    @RequestMapping(value = "/getAllType", method = RequestMethod.GET)
    public ResponseResult<Map> getAllType(HttpServletRequest request, Page page) {

        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader("sysToken"));*/
        AuthPlatformUserInfo userInfo = userService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("获取用户信息失败");
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO,ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        try {
            /**
             * USE, //使用指南
             * TRAINING, //内部培训
             * PLATFORM_INFO, //平台信息
             * RULES  //规章制度
             */
            ResponseResult<List<LearnDTO>> useMessage = learnService.getAllByType(userInfo, ConstInfos.LEARN_TYPE.USE.ordinal(), page);
            ResponseResult<List<LearnDTO>> trainingMessage = learnService.getAllByType(userInfo, ConstInfos.LEARN_TYPE.TRAINING.ordinal(), page);
            ResponseResult<List<LearnDTO>> platformInfoMessage = learnService.getAllByType(userInfo, ConstInfos.LEARN_TYPE.PLATFORM_INFO.ordinal(), page);
            ResponseResult<List<LearnDTO>> rulesMessage = learnService.getAllByType(userInfo, ConstInfos.LEARN_TYPE.RULES.ordinal(), page);

            Map<String, Object> returnMap = Maps.newHashMap();
            returnMap.put("0", useMessage);
            returnMap.put("1", trainingMessage);
            returnMap.put("2", platformInfoMessage);
            returnMap.put("3", rulesMessage);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, returnMap);
        } catch (Exception e) {
            log.error("获取学习指南列表失败，异常信息是：{}", e);
            return ResponseResult.buildResponseResult(ResCodeEnum.STUDY_GUIDE_LIST_NOT);
        }
    }
}
