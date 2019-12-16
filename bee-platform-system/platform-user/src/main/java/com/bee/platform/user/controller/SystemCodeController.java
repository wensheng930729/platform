package com.bee.platform.user.controller;


import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.SystemCode;
import com.bee.platform.common.service.SystemCodeService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.google.common.base.Preconditions;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author qhwang
 * @since 2019-03-04
 */
@RestController
@CrossOrigin(origins = "*")
@Api(value = "systemcode", tags = "公共码表接口")
@RequestMapping("/api/systemcode")
public class SystemCodeController {

    @Autowired
    private SystemCodeService systemCodeService;

    @ApiOperation(value = "获取某指定组列表信息")
    @RequestMapping(value = "/members", method = RequestMethod.GET)
    public ResponseResult<List<SystemCode>> members(@RequestParam String group) {
        Preconditions.checkArgument(StringUtils.isNotBlank(group), "group参数为空");
        try {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,systemCodeService.getCacheSysCodeInfo(ConstantsUtil.PLATFORM_SYSTEMCODE_KEY_PREFIX + group, group));
        } catch (Exception e) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
    }

}

