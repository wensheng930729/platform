package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName OrderDelRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/6/12$ 15:16$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("订单删除请求信息")
public class OrderDeleteRQ implements Serializable {

    private static final long serialVersionUID = -6440134713681133034L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("明细表id")
    private Integer subId;
}
