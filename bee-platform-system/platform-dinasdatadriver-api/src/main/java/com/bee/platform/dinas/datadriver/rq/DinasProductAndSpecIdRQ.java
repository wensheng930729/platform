package com.bee.platform.dinas.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @author liang.li
 * @ClassName DinasProductAndSpecIdRQ
 * @Description 产品id和规格id
 * @Date 2019-8-13
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "砂石产品id和规格id的DTO")
public class DinasProductAndSpecIdRQ implements Serializable {

    private static final long serialVersionUID = -2056179173858396800L;

    @ApiModelProperty("产品ID")
    @NotNull(message = "产品ID不能为空")
    private Integer productId;

    @ApiModelProperty("规格ID")
    @NotNull(message = "规格ID不能为空")
    private Integer productSpecId;


}
