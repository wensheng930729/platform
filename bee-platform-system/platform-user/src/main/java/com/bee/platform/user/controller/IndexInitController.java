package com.bee.platform.user.controller;


import com.bee.platform.business.dto.IndexInitDTO;
import com.bee.platform.business.rq.IndexInitRQ;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.service.IndexInitService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;

/**
 *
 * @author hongchuan.he123
 * @since 2019-03-20
 */
@Slf4j
@Api(value = "临时数据API", tags = "临时数据API")
@RestController
@RequestMapping("/number")
@CrossOrigin(origins = "*")
public class IndexInitController {

    @Autowired
    private IndexInitService indexInitService;

    @ApiOperation(value="首页临时定死的数据展示----后台接口")
    @RequestMapping(value = "/getAllNumbers",method = RequestMethod.GET)
    public ResponseResult<IndexInitDTO> getAllNumbers(){
        return  indexInitService.findAllNumbers();
    }
    
    @ApiOperation(value="修改临时定死的数据-----后台接口")
    @RequestMapping(value = "/modifyAllNumbers",method = RequestMethod.POST)
    public ResponseResult modifyAllNumbers(HttpServletRequest request,@RequestBody IndexInitRQ rq){
        return  indexInitService.modifyAllNumbers(rq);
    }
}

