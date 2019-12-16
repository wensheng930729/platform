package com.bee.platform.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.datadriver.dto.ErpRepositoryBoxDTO;
import com.bee.platform.datadriver.dto.ErpRepositoryDetailsDTO;
import com.bee.platform.datadriver.dto.ErpRepositoryListDTO;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpRepositoryAddRQ;
import com.bee.platform.datadriver.rq.ErpRepositoryDeleteRQ;
import com.bee.platform.datadriver.rq.ErpRepositoryEnableRQ;
import com.bee.platform.datadriver.rq.ErpRepositoryUpdataRQ;
import com.bee.platform.datadriver.service.ErpRepositoryService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 仓库档案 前端控制器
 * </p>
 *
 * @author hongchuan.he
 * @since 2019-05-27
 */
@Slf4j
@RestController
@RequestMapping("/erpRepository")
@Api(value = "仓库档案相关接口", tags = "仓库档案相关接口")
public class ErpRepositoryController {

    @Autowired
    private AuthUserFeignClient userInfoFeignClient;
    @Autowired
    private ErpRepositoryService erpRepositoryService;
    @Autowired
    private UserInfoUtils userInfoUtils;


    @PostMapping("/query")
    @ApiOperation(value = "列表")
    public ResponseResult<List<ErpRepositoryListDTO>> query(HttpServletRequest request, Page page,@RequestParam(value = "status", required = false) Integer status) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(request.getHeader("sysToken")).getObject();
//        Integer orgId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
//        if (ObjectUtils.isEmpty(orgId)) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
//        }
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        Integer orgId = userInfo.getOrgId();
        return erpRepositoryService.query(pagination, orgId, status);
    }

    @PostMapping("/updateErpRepositoryEnable")
    @ApiOperation(value = "启用禁用")
    @Log(businessType = EnumBusinessType.STOREHOUSE_PROFILE, operateType = OperateType.EDIT)
    public ResponseResult<Integer> updateErpRepositoryEnable(@RequestHeader("sysToken") String sysToken, @RequestBody ErpRepositoryEnableRQ enableRQ) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        return erpRepositoryService.updateErpRepositoryEnable(userInfo, enableRQ);
    }

    @PostMapping("/deleteErpRepositoryEnable")
    @ApiOperation(value = "删除")
    @Log(businessType = EnumBusinessType.STOREHOUSE_PROFILE, operateType = OperateType.DELETE)
    public ResponseResult<Integer> deleteErpRepositoryEnable(@RequestHeader("sysToken") String sysToken, @RequestBody ErpRepositoryDeleteRQ deleteRQ) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        return erpRepositoryService.delete(userInfo, deleteRQ);
    }


    @PostMapping("/add")
    @ApiOperation(value = "添加")
    @Log(businessType = EnumBusinessType.STOREHOUSE_PROFILE, operateType = OperateType.ADD)
    public ResponseResult<Integer> add(@RequestHeader("sysToken") String sysToken, @RequestBody ErpRepositoryAddRQ addRQ) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        return erpRepositoryService.add(userInfo, addRQ);
    }

    @GetMapping("/get")
    @ApiOperation(value = "根据id查询单个仓库档案")
    public ResponseResult get(@RequestParam int id) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, erpRepositoryService.get(id));
    }


    @PostMapping("/update")
    @ApiOperation(value = "更新")
    @Log(businessType = EnumBusinessType.STOREHOUSE_PROFILE, operateType = OperateType.EDIT)
    public ResponseResult<Integer> update(@RequestHeader("sysToken") String sysToken, @RequestBody ErpRepositoryUpdataRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        return erpRepositoryService.update(userInfo, rq);
    }

    @ApiOperation(value = "根据类型查询当前登录用户企业的仓库信息")
    @PostMapping("/getRepositoryList")
    public ResponseResult<List<ErpRepositoryBoxDTO>> getRepositoryList(HttpServletRequest request, @RequestBody(required = false) List<String> categoryList) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        String sysToken = request.getHeader("sysToken");
        List<ErpRepositoryBoxDTO> dto = erpRepositoryService.getRepositoryList(userInfo, sysToken,categoryList);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }

    @PostMapping("/getStatus")
    @ApiOperation(value = "根据状态查询仓库档案")
    public ResponseResult<List<ErpRepositoryDetailsDTO>> getStatus(HttpServletRequest request, Page page, Integer status) {
//        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
//        if (ObjectUtils.isEmpty(companyId)) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
//        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        Integer orgId = userInfo.getOrgId();
        return erpRepositoryService.getStatus(pagination, orgId, status);
    }
}

