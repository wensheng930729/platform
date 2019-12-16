package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @ClassName OpeningInventoryOrderQueryRQ
 * @Description 期初库存主表查询请求信息
 * @author jie.chen
 * @Date 2019/5/30$ 10:35$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("期初库存主表查询请求信息")
public class OpeningInventoryOrderQueryRQ implements Serializable {

    private static final long serialVersionUID = 4410762006817182677L;


    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("开始时间")
    private String startTime;

    @ApiModelProperty("截止时间")
    private String endTime;

    @ApiModelProperty("企业id集合")
    private List<Integer> list;
}
