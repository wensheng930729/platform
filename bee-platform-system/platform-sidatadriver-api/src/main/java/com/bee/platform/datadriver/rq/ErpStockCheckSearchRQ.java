package com.bee.platform.datadriver.rq;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * <p>
 * 库存盘点主单表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("库存盘点条件搜索请求参数")
public class ErpStockCheckSearchRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 库存盘点名称
     */

    @ApiModelProperty("库存盘点名称")
    private String stockCheckName;

    /**
     * 公司名称
     */
//    @ApiModelProperty("公司名称")
//    private String companyName;

    @ApiModelProperty("公司名称")
    private Integer companyId;

    @ApiModelProperty("开始时间")
    private String startTime;

    @ApiModelProperty("结束时间")
    private String endTime;

    private List<Integer> list;


}
