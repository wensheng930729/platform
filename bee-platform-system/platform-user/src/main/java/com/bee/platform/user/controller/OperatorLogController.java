package com.bee.platform.user.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.dto.OperatorLogDTO;
import com.bee.platform.user.service.OperatorLogService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * @notes  金蜜云平台后台操作日志相关接口
 * @Author junyang.li
 * @Date 17:07 2019/4/30
 **/
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/manager")
@Api(tags = "后台管理——日志接口")
public class OperatorLogController {

    @Autowired
    private OperatorLogService operatorLogService;

    @GetMapping("/operatorLogs")
    @ApiOperation("后台操作日志列表查询")
    public ResponseResult<List<OperatorLogDTO>> operatorLog(Page page){
        Pagination pagination=PageUtils.transFromPage(page);
        return operatorLogService.list(pagination);
    }

}

