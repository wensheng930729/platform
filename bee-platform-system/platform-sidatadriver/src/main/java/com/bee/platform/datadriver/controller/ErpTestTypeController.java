package com.bee.platform.datadriver.controller;


import java.util.List;

import javax.servlet.http.HttpServletRequest;

import com.bee.platform.datadriver.dto.ErpTestTypeDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.entity.ErpTestType;
import com.bee.platform.datadriver.rq.TestReportTypeRQ;
import com.bee.platform.datadriver.service.ErpTestTypeService;
import com.bee.platform.user.service.feign.AuthUserFeignClient;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;

/**
 * <p>
 * 化验类型 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpTestType", tags = "erp化验类型相关接口")
@RequestMapping("/erpTestType")
public class ErpTestTypeController {

    @Autowired
    private ErpTestTypeService testTypeService;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @PostMapping("/listErpTestType")
    @ApiOperation(value = "分页查询化验类型列表")
    public ResponseResult<List<ErpTestTypeDTO>> listErpTestType(HttpServletRequest request, @RequestBody TestReportTypeRQ rq, Page page){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)){
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        List<ErpTestTypeDTO> list = testTypeService.listErpTestType(userInfo, rq, pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
    }

    @GetMapping("/deleteTestType")
    @ApiOperation(value = "删除化验类型")
    public ResponseResult<Integer> deleteTestType(HttpServletRequest request, @RequestParam String id){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return testTypeService.deleteTestType(userInfo,id);
    }

    @PostMapping("/saveTestType")
    @ApiOperation(value = "保存化验单类型")
    public ResponseResult<Integer> saveTestType(HttpServletRequest request, @RequestBody TestReportTypeRQ rq){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return testTypeService.saveTestType(userInfo,rq);
    }
    @PostMapping("/getTestType")
    @ApiOperation(value = "化验类型")
    public ResponseResult<List<ErpTestType>> getTestType(HttpServletRequest request){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
		return testTypeService.getTestType(userInfo.getOrgId());
    }
}

